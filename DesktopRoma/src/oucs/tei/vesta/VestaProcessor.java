package oucs.tei.vesta;

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
import org.tei.iso.converter.DocX;
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
import com.thaiopensource.relaxng.output.xsd.XsdOutputFormat;
import com.thaiopensource.relaxng.translate.util.InvalidParamsException;
import com.thaiopensource.util.UriOrFile;
import com.thaiopensource.xml.sax.ErrorHandlerImpl;

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
	
	private static final String P5SUBSET = "../../local/p5subset.xml";
	private static final String SCHEMA_DIRECTORY = "resources/stylesheets/odds2";
	private static final String CSS_DIRECTORY = "resources/stylesheets";
    private static final String DEFAULT_OUTPUT_ENCODING = "UTF-8";
    private static final int DEFAULT_LINE_LENGTH = 72;
    private static final int DEFAULT_INDENT = 2;	
	
    private final RunDialog runDialog;
    private final Shell shell;
    private String baseDir;
	private boolean useCompiledODD;
    
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
		File outputDir = new File(this.outputDir);
		if(! inputFile.exists() )
			throw new IllegalArgumentException("The selected odd file does not exist.");
		if(! outputDir.exists() || ! outputDir.isDirectory())
			throw new IllegalArgumentException("The selected output directory does not exist or is not a directory.");
		
		// read input file in DomDocument
		XdmNode inputDocument = null;
		try {
			inputDocument = Utils.readFileIntoSaxonDoc(inputFile);
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
		XdmNode oddDocument = null;
		if(useCompiledODD){
			try {
				 appendInfo("Create compiled odd");
				 oddDocument = generateODD(inputDocument);
			} catch (Exception e) {
				throw new IllegalArgumentException("Could not run odd2odd transformation: " + e.getMessage());
			}
	
			// store file
			if(compile || debug){
				File compiledODDFile = new File(outputDir + File.separator + schemaName + ".odd");
				Utils.storeSaxonDoc(oddDocument, compiledODDFile);
			}
			
			// generate Relax
			XdmNode relaxDocument = null;
			File relaxFile = null;
			if(generateRNG || generateXSD){
				try {
					appendInfo("Create Relax NG");
					relaxDocument = generateRelax(oddDocument);
				} catch (Exception e) {
					throw new IllegalArgumentException("Could not run odd2relax transformation: " + e.getMessage());		
				}
				
				// store file
				relaxFile = new File(outputDir + File.separator + schemaName + ".rng");
				Utils.storeSaxonDoc(relaxDocument, relaxFile);
			}
	
			// generate dtd
			if(generateDTD){
				try {
					appendInfo("Create DTD");
					generateDTD(oddDocument);
				} catch (SaxonApiException e) {
					throw new IllegalArgumentException("Could not run odd2dtd transformation: " + e.getMessage());
				}
			}
			
			// run trang
			try {
				// generate compact relax
				if(generateRNG){
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
		
		
		XdmNode teiDocumentation = null;
		if(useCompiledODD){
			// create the TEI documentation if it we are supposed to create it or the docx documentation
			if(documentationTEI || documentationDocX){
				try {
					teiDocumentation = generateTEIDocumentation(oddDocument);
				} catch (SaxonApiException e) {
					appendInfo("Error: Could not create TEI documentation: " + e.getMessage());
				}
			}
			
			// store file
			if(documentationTEI){
				appendInfo("Generate Documentation (TEI)");
				File teiDocFile = new File(outputDocDir + File.separator + schemaName + ".xml");
				Utils.storeSaxonDoc(teiDocumentation, teiDocFile);
			}
			
			if(documentationHTML){
				try{
					appendInfo("Generate Documentation (HTML)");
					generateHTMLDocumentation(oddDocument);
				} catch (Exception e) {
					appendInfo("Error: Could not create HTML documentation: " + e.getMessage());
				}
			}
		} 
		
		
		if(documentationDocX && useCompiledODD){
			appendInfo("Generate Documentation (docx)");
			try {
				generateDocXDocumentation(teiDocumentation);
			} catch (SaxonApiException e) {
				appendInfo("Error: Could not create docx documentation: " + e.getMessage());
			}
		} else if(documentationDocX){
			appendInfo("Generate docx file from: " + inputFile);
			try {
				generateDocXDocumentation(inputDocument);
			} catch (SaxonApiException e) {
				appendInfo("Error: Could not create docx file: " + e.getMessage());
			}
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


	private XdmNode generateODD(XdmNode doc) throws TransformerException, SaxonApiException {
		// load stylesheets
		Processor proc = SaxonProcFactory.getProcessor();
		
		// prepare transformer
		XsltCompiler comp = proc.newXsltCompiler();
		comp.setErrorListener(this);
		XsltExecutable odd2oddExec = comp.compile(new StreamSource(baseDir + SCHEMA_DIRECTORY + File.separator + "odd2odd.xsl"));
		XsltTransformer odd2oddTransformer = odd2oddExec.load();
		
		odd2oddTransformer.setParameter(new QName("selectedSchema"), new XdmAtomicValue(schemaName));
		
		if(debug)
			odd2oddTransformer.setParameter(new QName("verbose"), new XdmAtomicValue("true"));
		if(compile)
			odd2oddTransformer.setParameter(new QName("stripped"), new XdmAtomicValue("true"));
		
		
		odd2oddTransformer.setParameter(new QName("localsource"), new XdmAtomicValue(P5SUBSET));
		odd2oddTransformer.setParameter(new QName("TEIC"), new XdmAtomicValue("true") );
		odd2oddTransformer.setParameter(new QName("lang"), new XdmAtomicValue(language));
		odd2oddTransformer.setParameter(new QName("doclang"), new XdmAtomicValue(language));
		odd2oddTransformer.setParameter(new QName("useVersionFromTEI"), new XdmAtomicValue( useVersionFromTEI ? "true" : "false" ));
		
		odd2oddTransformer.setMessageListener(this);
		odd2oddTransformer.setInitialContextNode(doc);
		XdmDestination result = new XdmDestination();
		odd2oddTransformer.setDestination(result);
		odd2oddTransformer.transform();
		
		return (XdmNode) result.getXdmNode();
	}
	
	private XdmNode generateRelax(XdmNode doc) throws TransformerException, SaxonApiException{
		// load stylesheets
		Processor proc = SaxonProcFactory.getProcessor();
		
		// prepare transformer
		XsltCompiler comp = proc.newXsltCompiler();
		comp.setErrorListener(this);
		XsltExecutable odd2relaxExec = comp.compile(new StreamSource(baseDir + SCHEMA_DIRECTORY + File.separator + "odd2relax.xsl"));
		XsltTransformer odd2relaxTransformer = odd2relaxExec.load();
		
		if(debug)
			odd2relaxTransformer.setParameter(new QName("verbose"), new XdmAtomicValue("true"));

		odd2relaxTransformer.setParameter(new QName("TEIC"), new XdmAtomicValue("true") );
		odd2relaxTransformer.setParameter(new QName("lang"), new XdmAtomicValue(language));
		odd2relaxTransformer.setParameter(new QName("doclang"), new XdmAtomicValue(language));
		odd2relaxTransformer.setParameter(new QName("parameterize"), new XdmAtomicValue(parameterizedDTD ? "true" : "false"));
		odd2relaxTransformer.setParameter(new QName("patternPrefix"), new XdmAtomicValue(patternPrefix));
		
		odd2relaxTransformer.setMessageListener(this);
		odd2relaxTransformer.setInitialContextNode(doc);
		XdmDestination result = new XdmDestination();
		odd2relaxTransformer.setDestination(result);
		odd2relaxTransformer.transform();
		
		return (XdmNode) result.getXdmNode();
	}
	
	public void generateDTD(XdmNode doc) throws SaxonApiException{
		// load stylesheets
		Processor proc = SaxonProcFactory.getProcessor();
		
		// prepare transformer
		XsltCompiler comp = proc.newXsltCompiler();
		comp.setErrorListener(this);
		XsltExecutable odd2dtdExec = comp.compile(new StreamSource(baseDir + SCHEMA_DIRECTORY + File.separator + "odd2dtd.xsl"));
		XsltTransformer odd2dtdTransformer = odd2dtdExec.load();
		
		if(debug)
			odd2dtdTransformer.setParameter(new QName("verbose"), new XdmAtomicValue("true"));
		
		odd2dtdTransformer.setParameter(new QName("TEIC"), new XdmAtomicValue("true") );
		odd2dtdTransformer.setParameter(new QName("lang"), new XdmAtomicValue(language));
		odd2dtdTransformer.setParameter(new QName("doclang"), new XdmAtomicValue(language));
		odd2dtdTransformer.setParameter(new QName("parameterize"), new XdmAtomicValue(parameterizedDTD ? "true" : "false"));
		
		odd2dtdTransformer.setMessageListener(this);
		odd2dtdTransformer.setInitialContextNode(doc);
		Serializer result = new Serializer();
		result.setOutputFile(new File(outputDir + File.separator + schemaName + ".dtd") );
		odd2dtdTransformer.setDestination(result);
		odd2dtdTransformer.transform();
	}
	
	public void generateRelaxCompact(File input) throws InputFailedException, InvalidParamsException, IOException, SAXException, OutputFailedException{
		InputFormat inFormat = new SAXParseInputFormat();
		OutputFormat of = new XsdOutputFormat();
		
		String[] inputParamArray = new String[]{};
		String[] outputParamArray = new String[]{"disable-abstract-elements"};
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
	
	public XdmNode generateTEIDocumentation(XdmNode doc) throws SaxonApiException{
		// load stylesheets
		Processor proc = SaxonProcFactory.getProcessor();
		
		// prepare transformer
		XsltCompiler comp = proc.newXsltCompiler();
		comp.setErrorListener(this);
		XsltExecutable odd2teiDocExec = comp.compile(new StreamSource(baseDir + SCHEMA_DIRECTORY + File.separator + "odd2lite.xsl"));
		XsltTransformer odd2teiDocTransformer = odd2teiDocExec.load();
		
		
		odd2teiDocTransformer.setParameter(new QName("TEIC"), new XdmAtomicValue("true") );
		odd2teiDocTransformer.setParameter(new QName("localsource"), new XdmAtomicValue(P5SUBSET));
		odd2teiDocTransformer.setParameter(new QName("lang"), new XdmAtomicValue(language));
		odd2teiDocTransformer.setParameter(new QName("doclang"), new XdmAtomicValue(language));
		
		odd2teiDocTransformer.setMessageListener(this);
		odd2teiDocTransformer.setInitialContextNode(doc);
		XdmDestination result = new XdmDestination();
		odd2teiDocTransformer.setDestination(result);
		odd2teiDocTransformer.transform();
		
		return (XdmNode) result.getXdmNode();
	}
	
	private void generateDocXDocumentation(XdmNode teiDocumentation) throws SaxonApiException {
		DocX docx = new DocX(schemaName, null);
		docx.mergeTEI(teiDocumentation);
		File zipFile = docx.getDocXFile();
		zipFile.renameTo(new File(outputDocDir + File.separator + schemaName + ".docx"));
		docx.cleanUp();
	}	
	
	public void generateHTMLDocumentation(XdmNode doc) throws SaxonApiException, IOException{
		// load stylesheets
		Processor proc = SaxonProcFactory.getProcessor();
		
		// prepare transformer
		XsltCompiler comp = proc.newXsltCompiler();
		comp.setErrorListener(this);
		XsltExecutable odd2HTMLDocExec = comp.compile(new StreamSource(baseDir + SCHEMA_DIRECTORY + File.separator + "odd2html.xsl"));
		XsltTransformer odd2HTMLDocTransformer = odd2HTMLDocExec.load();
		
		odd2HTMLDocTransformer.setParameter(new QName("STDOUT"), new XdmAtomicValue("true") );
		odd2HTMLDocTransformer.setParameter(new QName("splitLevel"), new XdmAtomicValue("-1"));
		odd2HTMLDocTransformer.setParameter(new QName("lang"), new XdmAtomicValue(language));
		odd2HTMLDocTransformer.setParameter(new QName("doclang"), new XdmAtomicValue(language));
		odd2HTMLDocTransformer.setParameter(new QName("documentationLanguage"), new XdmAtomicValue(language));
		odd2HTMLDocTransformer.setParameter(new QName("cssFile"), new XdmAtomicValue("tei.css"));
		odd2HTMLDocTransformer.setParameter(new QName("cssSecondaryFile"), new XdmAtomicValue("odd.css"));
		
		odd2HTMLDocTransformer.setMessageListener(this);
		odd2HTMLDocTransformer.setInitialContextNode(doc);
		Serializer result = new Serializer();
		result.setOutputFile(new File(outputDocDir + File.separator + schemaName + ".html") );
		odd2HTMLDocTransformer.setDestination(result);
		odd2HTMLDocTransformer.transform();
		
		// copy css
		Utils.copyFile(new File(baseDir + CSS_DIRECTORY + File.separator + "tei.css"), new File( outputDocDir + File.separator + "tei.css") );
		Utils.copyFile(new File(baseDir + CSS_DIRECTORY + File.separator + "odd.css"), new File( outputDocDir + File.separator + "odd.css") );
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
