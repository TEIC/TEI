package org.tei.vesta;

import java.io.File;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.ArrayList;
import java.util.List;

import javax.xml.transform.ErrorListener;
import javax.xml.transform.SourceLocator;
import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamSource;

import net.sf.saxon.om.NodeInfo;
import net.sf.saxon.s9api.MessageListener;
import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.QName;
import net.sf.saxon.s9api.SaxonApiException;
import net.sf.saxon.s9api.Serializer;
import net.sf.saxon.s9api.XdmAtomicValue;
import net.sf.saxon.s9api.XdmDestination;
import net.sf.saxon.s9api.XdmNode;
import net.sf.saxon.s9api.XsltCompiler;
import net.sf.saxon.s9api.XsltExecutable;
import net.sf.saxon.s9api.XsltTransformer;
import net.sf.saxon.sxpath.XPathEvaluator;
import net.sf.saxon.sxpath.XPathExpression;
import net.sf.saxon.trans.XPathException;

import org.eclipse.swt.SWT;
import org.eclipse.swt.events.ShellEvent;
import org.eclipse.swt.events.ShellListener;
import org.eclipse.swt.graphics.Cursor;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Event;
import org.eclipse.swt.widgets.Listener;
import org.eclipse.swt.widgets.MessageBox;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.swt.widgets.Text;
import org.tei.docx.DocX;
import org.tei.exceptions.ConfigurationException;
import org.tei.tei.DTDGenerationProperties;
import org.tei.tei.DocXTransformationProperties;
import org.tei.tei.DocumentationGenerationProperties;
import org.tei.tei.HTMLTransformationProperties;
import org.tei.tei.ODDGenerationProperties;
import org.tei.tei.RelaxGenerationProperties;
import org.tei.tei.TEI;
import org.tei.utils.FileUtils;
import org.tei.utils.SaxonProcFactory;
import org.tei.utils.XMLUtils;
import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

import com.thaiopensource.relaxng.edit.SchemaCollection;
import com.thaiopensource.relaxng.input.InputFailedException;
import com.thaiopensource.relaxng.input.InputFormat;
import com.thaiopensource.relaxng.input.parse.sax.SAXParseInputFormat;
import com.thaiopensource.relaxng.output.LocalOutputDirectory;
import com.thaiopensource.relaxng.output.OutputDirectory;
import com.thaiopensource.relaxng.output.OutputFailedException;
import com.thaiopensource.relaxng.output.OutputFormat;
import com.thaiopensource.relaxng.output.rnc.RncOutputFormat;
import com.thaiopensource.relaxng.output.xsd.XsdOutputFormat;
import com.thaiopensource.relaxng.translate.util.InvalidParamsException;
import com.thaiopensource.util.UriOrFile;

public class VestaProcessor implements Runnable, ErrorListener, ErrorHandler, MessageListener{

	private String oddFile;
	private String outputDir;
	private String outputDocDir;
	private String schemaName = "default";
	private String language = "en";
	
	private boolean generateXSD = true;
	private boolean generateDTD = true;
	private boolean generateRNG = true;
	
	private boolean documentationTEI = false;
	private boolean documentationHTML = false;
	private boolean documentationDocX = false;
	
	private boolean compile = false;
	private boolean debug = false;
	
	private boolean useVersionFromTEI = true;
	private boolean parameterizedDTD = false;
	private String patternPrefix = "";
	
	
	private static final String DEFAULT_OUTPUT_ENCODING = "UTF-8";
    private static final int DEFAULT_LINE_LENGTH = 72;
    private static final int DEFAULT_INDENT = 2;	
	
    private final RunDialog runDialog;
    private final Shell shell;
    private String baseDir;
	private boolean useCompiledODD;
	private PropertiesProvider properties;
    
	public VestaProcessor(){
		shell = new Shell(Display.getDefault());
		runDialog = new RunDialog(shell, SWT.RESIZE | SWT.NO_TRIM | SWT.CLOSE);
		
		baseDir = VestaProcessor.class.getProtectionDomain().getCodeSource().getLocation().getPath();
		baseDir = baseDir.substring(0, baseDir.lastIndexOf(File.separator));
		File baseDirFile = new File(baseDir + File.separator + "resources");
		if(!baseDirFile.exists())
			baseDir = baseDir.substring(0, baseDir.lastIndexOf(File.separator));
		baseDir += File.separator;
		
		try {
			baseDir = URLDecoder.decode(baseDir,"UTF-8");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
		
		// load properties
		this.properties = PropertiesProvider.getInstance();
	}
	
	
	/**
	 * start processing
	 * @throws MoreThanOneSchemaSpecException 
	 */
	public void run() throws IllegalArgumentException{
		boolean bErrror = false;
		
		Display.getDefault().asyncExec(new Runnable(){
			public void run() {
				runDialog.open();
				runDialog.appendLine("Start processing " + oddFile);
			}
			
		});
		
		try{
			process();
		} catch(final Exception e){
			e.printStackTrace();
			Display.getDefault().asyncExec( new Runnable() {
				public void run() {
					MessageBox mb = new MessageBox(shell);
					mb.setMessage(e.getMessage());
					mb.open();
				}
			} );
			
			bErrror = true;
		}
		
		if(!bErrror){
			Display.getDefault().asyncExec( new Runnable() {
				public void run(){
					MessageBox mb = new MessageBox(shell);
					mb.setMessage("Done");
					mb.open();
				}
			});
		} else {
			Display.getDefault().asyncExec( new Runnable() {
				public void run(){
					MessageBox mb = new MessageBox(shell);
					mb.setMessage("Errors occured while processing " + oddFile);
					mb.open();
				}
			});
		}
		
		runDialog.setAllowClose(true);
	}
	
	private void process() throws IllegalArgumentException{
		// load file
		File inputFile = new File(oddFile);
		final File outputDir = new File(this.outputDir);
		if(! inputFile.exists() )
			throw new IllegalArgumentException("The selected odd file does not exist.");
		if(! outputDir.exists() || ! outputDir.isDirectory())
			throw new IllegalArgumentException("The selected output directory does not exist or is not a directory.");
		
		// set properties provider in TEI
		TEI.setPropertiesProvider(properties);
		
		// read input file in DomDocument
		TEI tei = null;
		try {
			tei = new TEI(inputFile);
		} catch(SaxonApiException e){
			Throwable nested = e.getCause();
			if(nested instanceof XPathException)
				throw new IllegalArgumentException("Could not parse input file: " + ((XPathException)nested).getMessageAndLocation());
			else
				throw new IllegalArgumentException("Could not parse input file: " + e.getMessage());
		} catch (Exception e) {
			throw new IllegalArgumentException("Could not parse input file: " + e.getMessage());
		}
		
		// generateODD
		TEI oddDocument = null;
		if(useCompiledODD){
			try {
				 appendInfo("Create compiled odd");
				 oddDocument = tei.generateODD(new ODDGenerationProperties(){
					@Override
					public String getSchemaName() {
						return schemaName;
					}
					
					@Override
					public boolean isStripped(){
						return compile;
					}
					
					@Override
					public boolean isVerbose(){
						return debug;
					}
					
					@Override
					public String getLanguage(){
						return language;
					}
					
					@Override
					public boolean isUseVersionFromTEI(){
						return useVersionFromTEI;
					}
					
					@Override
					public ErrorListener getErrorListener() {
						return VestaProcessor.this;
					}
					
					@Override
					public MessageListener getMessageListener() {
						return VestaProcessor.this;
					} 
					 
				 });
			} catch (Exception e) {
				throw new IllegalArgumentException("Could not run odd2odd transformation: " + e.getMessage());
			}
	
			// store file
			if(compile || debug){
				try {
					oddDocument.storeFile(new File(outputDir + File.separator + schemaName + ".odd"));
				} catch (IOException e) {
					appendInfo("Error: Could not store compiled ODD file: " + e.getMessage());
				}
			}
			
			
			
			// generate Relax
			XdmNode relaxDocument = null;
			File relaxFile = null;
			if(generateRNG || generateXSD){
				try {
					appendInfo("Create Relax NG");
					relaxDocument = oddDocument.generateRelax(new RelaxGenerationProperties(){
						@Override
						public String getSchemaName() {
							return schemaName;
						}
						
						@Override
						public boolean isVerbose(){
							return debug;
						}
						
						@Override
						public String getLanguage(){
							return language;
						}
						
						@Override
						public String getPatternPrefix(){
							return patternPrefix;
						}
						
						@Override
						public boolean isParameterizedDTD(){
							return parameterizedDTD;
						}
						
						@Override
						public ErrorListener getErrorListener() {
							return VestaProcessor.this;
						}
						
						@Override
						public MessageListener getMessageListener() {
							return VestaProcessor.this;
						} 
					});
				} catch (Exception e) {
					throw new IllegalArgumentException("Could not run odd2relax transformation: " + e.getMessage());		
				}
				
				// store file
				relaxFile = new File(outputDir + File.separator + schemaName + ".rng");
				try {
					XMLUtils.storeDocument(relaxDocument, relaxFile);
				} catch (IOException e) {
					appendInfo("Error: Could not store relaxNG schema: " + e.getMessage());
				}
			}
	
			// generate dtd
			if(generateDTD){
				try {
					appendInfo("Create DTD");
					oddDocument.generateDTD(new DTDGenerationProperties(){

						@Override
						public File getOutputFile() {
							return new File(outputDir + File.separator + schemaName + ".dtd");
						}

						@Override
						public String getSchemaName() {
							return schemaName;
						}
						
						@Override
						public boolean isVerbose(){
							return debug;
						}
						
						@Override
						public String getLanguage(){
							return language;
						}
						
						@Override
						public boolean isParameterizedDTD(){
							return parameterizedDTD;
						}
						
						@Override
						public ErrorListener getErrorListener() {
							return VestaProcessor.this;
						}
						
						@Override
						public MessageListener getMessageListener() {
							return VestaProcessor.this;
						} 
						
					});
				} catch (SaxonApiException e) {
					throw new IllegalArgumentException("Could not run odd2dtd transformation: " + e.getMessage());
				}
			}
			
			// run trang
			try {
				// generate compact relax
				if(generateRNG){
					System.out.println(relaxFile);
					appendInfo("Create Compact Relax NG");
					generateRelaxCompact(relaxFile);
				}
				// generate xsd
				if(generateXSD){
					appendInfo("Create XSD");
					generateXSD(relaxFile);
				}
			} catch (Exception e) {
				throw new IllegalArgumentException("Could not run trang: " + e.getMessage());
			}
			
			// free memory
			relaxDocument = null;
			
			// clean up?
			if(! debug ){
				if(! generateXSD && null != relaxFile){
					relaxFile.delete();
				}
			}
		}
		
		// documentation
		if(documentationDocX || documentationHTML || documentationTEI){
			appendInfo("Generate Documentation");
			File docDir = new File(outputDocDir);
			if(! docDir.isDirectory() && docDir.exists())
				throw new IllegalArgumentException("Could not create directory: " + docDir.getAbsolutePath());
			if(! docDir.isDirectory() )
				docDir.mkdir();
		}
		
		
		TEI teiDocumentation = null;
		if(useCompiledODD){
			// create the TEI documentation if it we are supposed to create it or the docx documentation
			if(documentationTEI || documentationDocX){
				try {
					teiDocumentation = oddDocument.generateDocumentation(new DocumentationGenerationProperties(){
						public ErrorListener getErrorListener() {
							return VestaProcessor.this;
						}

						public MessageListener getMessageListener() {
							return VestaProcessor.this;
						} 
						
						public String getLanguage(){
							return language;
						}
					});
				} catch (SaxonApiException e) {
					appendInfo("Error: Could not create TEI documentation: " + e.getMessage());
				}
			}
			
			// store file
			if(documentationTEI){
				appendInfo("Generate Documentation (TEI)");
				try {
					teiDocumentation.storeFile(new File(outputDocDir + File.separator + schemaName + ".xml"));
				} catch (IOException e) {
					appendInfo("Error: Could not store TEI documentation: " + e.getMessage());
				}
			}
			
			if(documentationHTML){
				appendInfo("Generate Documentation (HTML)");
				transformToHTML(oddDocument);
			}
		} else if(documentationHTML){
			appendInfo("Generate HTML file");
			transformToHTML(tei);
		}
		
		
		
		if(documentationDocX && useCompiledODD){
			appendInfo("Generate Documentation (docx)");
			transformToDocX(teiDocumentation);
		} else if(documentationDocX){
			appendInfo("Generate docx file from: " + inputFile);
			transformToDocX(tei);
		}
		
		
		//
		appendInfo("done");
	}
	

	private void appendInfo(final String text) {
		if(! runDialog.getParent().isDisposed()){
			Display.getDefault().asyncExec( new Runnable() {
				public void run() {
					runDialog.appendLine(text);
				}
			} );
		}
	}
	
	
	private void transformToHTML(TEI doc){
		try{
			doc.transformToHTML(new HTMLTransformationProperties(){
				@Override
				public ErrorListener getErrorListener() {
					return VestaProcessor.this;
				}
				
				@Override
				public MessageListener getMessageListener() {
					return VestaProcessor.this;
				} 
				
				@Override
				public File getOutputFile() {
					return new File(outputDocDir + File.separator + schemaName + ".html");
				}
				
				@Override
				public String getCSSFile(){
					return "tei.css";
				}
				
				@Override
				public String getCSSSecondaryFile(){
					return "odd.css";
				}
			});

			// copy css
			FileUtils.copyFile(new File(PropertiesProvider.getInstance().getCSSDir() + File.separator + "tei.css"), new File( outputDocDir + File.separator + "tei.css") );
			FileUtils.copyFile(new File(PropertiesProvider.getInstance().getCSSDir() + File.separator + "odd.css"), new File( outputDocDir + File.separator + "odd.css") );
		} catch (Exception e) {
			appendInfo("Error: Could not create HTML file : " + e.getMessage());
		}
	}
	
	private void transformToDocX(TEI doc){
		try {
			doc.transformToDocX(new DocXTransformationProperties(){

				@Override
				public File getOutputFile() {
					return new File(outputDocDir + File.separator + schemaName + ".docx");
				}

				public String docx_pp_getDocXTemplateFile() {
					return properties.docx_pp_getDocXTemplateFile();
				}

				public String docx_pp_getStylesheetCheckDocx() {
					return properties.docx_pp_getStylesheetCheckDocx();
				}

				public String docx_pp_getStylesheetDocx2TEI() {
					return properties.docx_pp_getStylesheetDocx2TEI();
				}

				public String docx_pp_getStylesheetNormalizeWordStyles() {
					return properties.docx_pp_getStylesheetNormalizeWordStyles();
				}

				public String docx_pp_getStylesheetTEI2Docx() {
					return properties.docx_pp_getStylesheetTEI2Docx();
				}

				public String docx_pp_getTempDir() {
					return properties.docx_pp_getTempDir();
				}
				
			});
		} catch (Exception e) {
			appendInfo("Error: Could not create docx file: " + e.getMessage());
		}
	}


	
	public void generateRelaxCompact(File input) throws InputFailedException, InvalidParamsException, IOException, SAXException, OutputFailedException{
		InputFormat inFormat = new SAXParseInputFormat();
		OutputFormat of = new RncOutputFormat();
		String[] inputParamArray = new String[]{};
		String[] outputParamArray = new String[]{};
		SchemaCollection sc =  inFormat.load(UriOrFile.toUri(input.getAbsolutePath()), inputParamArray, "rnc", this);
		OutputDirectory od = new LocalOutputDirectory( 
				sc.getMainUri(),
				new File(outputDir + File.separator + schemaName + ".rnc"),
				"rnc",
				DEFAULT_OUTPUT_ENCODING,
                DEFAULT_LINE_LENGTH,
                DEFAULT_INDENT
        );
		of.output(sc, od, outputParamArray, "rng", this);
	}
	
	public void generateXSD(File input) throws InputFailedException, InvalidParamsException, IOException, SAXException, OutputFailedException{
		InputFormat inFormat = new SAXParseInputFormat();
		OutputFormat of = new XsdOutputFormat();
		
		String[] inputParamArray = new String[]{};
		String[] outputParamArray = new String[]{"disable-abstract-elements"};
		SchemaCollection sc =  inFormat.load(UriOrFile.toUri(input.getAbsolutePath()), inputParamArray, "xsd", this);
		
		OutputDirectory od = new LocalOutputDirectory( 
				sc.getMainUri(),
				new File(outputDir + File.separator + schemaName + ".xsd"),
				"xsd",
				DEFAULT_OUTPUT_ENCODING,
                DEFAULT_LINE_LENGTH,
                DEFAULT_INDENT
        );
		
		of.output(sc, od, outputParamArray, "rng", this);
	}
	

	public String getOddFile() {
		return oddFile;
	}



	public void setOddFile(String oddFile) {
		this.oddFile = oddFile;
	}



	public String getOutputDir() {
		return outputDir;
	}



	public void setOutputDir(String outputDir) {
		this.outputDir = outputDir;
		this.outputDocDir = outputDir + File.separator + "documentation";
	}


	public String getSchemaName() {
		return schemaName;
	}


	public void setSchemaName(String schemaName) {
		this.schemaName = schemaName;
	}


	public String getLanguage() {
		return language;
	}


	public void setLanguage(String language) {
		this.language = language;
	}


	public boolean isGenerateXSD() {
		return generateXSD;
	}


	public void setGenerateXSD(boolean generateXSD) {
		this.generateXSD = generateXSD;
	}


	public boolean isGenerateDTD() {
		return generateDTD;
	}


	public void setGenerateDTD(boolean generateDTD) {
		this.generateDTD = generateDTD;
	}


	public boolean isGenerateRNG() {
		return generateRNG;
	}


	public void setGenerateRNG(boolean generateRNG) {
		this.generateRNG = generateRNG;
	}


	public boolean isDocumentationTEI() {
		return documentationTEI;
	}


	public void setDocumentationTEI(boolean documentationTEI) {
		this.documentationTEI = documentationTEI;
	}


	public boolean isDocumentationHTML() {
		return documentationHTML;
	}


	public void setDocumentationHTML(boolean documentationHTML) {
		this.documentationHTML = documentationHTML;
	}


	public boolean isDocumentationDocX() {
		return documentationDocX;
	}


	public void setDocumentationDocX(boolean documentationDocX) {
		this.documentationDocX = documentationDocX;
	}


	public boolean isCompile() {
		return compile;
	}


	public void setCompile(boolean compile) {
		this.compile = compile;
	}


	public boolean isDebug() {
		return debug;
	}


	public void setDebug(boolean debug) {
		this.debug = debug;
	}


	public boolean isUseVersionFromTEI() {
		return useVersionFromTEI;
	}


	public void setUseVersionFromTEI(boolean useVersionFromTEI) {
		this.useVersionFromTEI = useVersionFromTEI;
	}


	public boolean isParameterizedDTD() {
		return parameterizedDTD;
	}


	public void setParameterizedDTD(boolean parameterizedDTD) {
		this.parameterizedDTD = parameterizedDTD;
	}


	public String getPatternPrefix() {
		return patternPrefix;
	}


	public void setPatternPrefix(String patternPrefix) {
		this.patternPrefix = patternPrefix;
	}

	public void setUseCompiledODD(boolean useCompiledODD) {
		this.useCompiledODD = useCompiledODD;
	}
	

	
	
	public void error(TransformerException exception)
			throws TransformerException {
		appendInfo("Error: " + exception.getMessage());
	}


	public void fatalError(TransformerException exception)
			throws TransformerException {
		appendInfo("Fatal Error: " + exception.getMessage());
		throw exception;
	}


	public void warning(TransformerException exception)
			throws TransformerException {
		appendInfo("Warning: " + exception.getMessage());	
	}


	public void error(SAXParseException exception) throws SAXException {
		appendInfo("Error: " + exception.getMessage());
	}


	public void fatalError(SAXParseException exception) throws SAXException {
		appendInfo("Fatal Error: " + exception.getMessage());
		throw exception;
	}


	public void warning(SAXParseException exception) throws SAXException {
		appendInfo("Warning: " + exception.getMessage());
	}


	public void message(XdmNode content, boolean terminate, SourceLocator locator) {
		appendInfo("Message: " + content.getStringValue());
		
	}
	

}
