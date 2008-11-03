package oucs.tei.vesta;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import javax.xml.transform.TransformerException;
import javax.xml.transform.stream.StreamSource;

import net.sf.saxon.om.NodeInfo;
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
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Text;
import org.xml.sax.SAXException;

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

public class VestaProcessor implements Runnable{

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
	private Text informationArea = null;
	
	private static final String P5SUBSET = "../../local/p5subset.xml";
	private static final String SCHEMA_DIRECTORY = "resources/stylesheets/odds2";
	private static final String CSS_DIRECTORY = "resources/stylesheets";
    private static final String DEFAULT_OUTPUT_ENCODING = "UTF-8";
    private static final int DEFAULT_LINE_LENGTH = 72;
    private static final int DEFAULT_INDENT = 2;	
	
    private String baseDir;
    
	public VestaProcessor(){
		baseDir = VestaProcessor.class.getProtectionDomain().getCodeSource().getLocation().getPath();
		baseDir = baseDir.substring(0, baseDir.lastIndexOf(File.separator));
		File baseDirFile = new File(baseDir + File.separator + "resources");
		if(!baseDirFile.exists())
			baseDir = baseDir.substring(0, baseDir.lastIndexOf(File.separator));
		baseDir += File.separator;
	}
	

	/**
	 * start processing
	 * @throws MoreThanOneSchemaSpecException 
	 */
	public void run() throws IllegalArgumentException{
		// load file
		File inputFile = new File(oddFile);
		File outputDir = new File(this.outputDir);
		if(! inputFile.exists() )
			throw new IllegalArgumentException("The selected odd file does not exist.");
		if(! outputDir.exists() || ! outputDir.isDirectory())
			throw new IllegalArgumentException("The selected output directory does not exist or is not a directory.");
		
		// read input file in DomDocument
		XdmNode doc = null;
		try {
			doc = Utils.readFileIntoSaxonDoc(inputFile);
		} catch (Exception e) {
			throw new IllegalArgumentException("Could not parse input file: " + e.getMessage());
		}
		
		// generateODD
		XdmNode oddDocument = null;
		try {
			 appendInfo("Create compiled odd");
			 oddDocument = generateODD(doc);
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
		
		// documentation
		if(documentationDocX || documentationHTML || documentationTEI){
			appendInfo("Generate Documentation");
			File docDir = new File(outputDocDir);
			if(! docDir.isDirectory() && docDir.exists())
				throw new IllegalArgumentException("Could not create directory: " + docDir.getAbsolutePath());
			if(! docDir.isDirectory() )
				docDir.mkdir();
		}
		
		if(documentationDocX)
			throw new IllegalArgumentException("The conversion to docx is not yet supported.");
		
		if(documentationTEI){
			try {
				generateTEIDocumentation(oddDocument);
			} catch (SaxonApiException e) {
				throw  new IllegalArgumentException("Could not create TEI documentation: " + e.getMessage());
			}
		}
		
		if(documentationHTML){
			try{
				generateHTMLDocumentation(oddDocument);
			} catch (Exception e) {
				throw  new IllegalArgumentException("Could not create HTML documentation: " + e.getMessage());
			}
		}
		
		
		// clean up?
		if(! debug ){
			if(! generateXSD && null != relaxFile){
				relaxFile.delete();
			}
		}
		
		//
		appendInfo("done");
	}
	
	private void appendInfo(final String text) {
		if(null != informationArea){
			Display.getDefault().asyncExec( new Runnable() {

				public void run() {
					informationArea.setText(informationArea.getText() + "\n" + text);
				}
				
			} );
			
		}
	}


	private XdmNode generateODD(XdmNode doc) throws TransformerException, SaxonApiException {
		// load stylesheets
		Processor proc = SaxonProcFactory.getProcessor();
		
		// prepare transformer
		XsltCompiler comp = proc.newXsltCompiler();
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
		XsltExecutable odd2relaxExec = comp.compile(new StreamSource(baseDir + SCHEMA_DIRECTORY + File.separator + "odd2relax.xsl"));
		XsltTransformer odd2relaxTransformer = odd2relaxExec.load();
		
		if(debug)
			odd2relaxTransformer.setParameter(new QName("verbose"), new XdmAtomicValue("true"));

		odd2relaxTransformer.setParameter(new QName("TEIC"), new XdmAtomicValue("true") );
		odd2relaxTransformer.setParameter(new QName("lang"), new XdmAtomicValue(language));
		odd2relaxTransformer.setParameter(new QName("doclang"), new XdmAtomicValue(language));
		odd2relaxTransformer.setParameter(new QName("parameterize"), new XdmAtomicValue(parameterizedDTD ? "true" : "false"));
		odd2relaxTransformer.setParameter(new QName("patternPrefix"), new XdmAtomicValue(patternPrefix));
		
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
		XsltExecutable odd2dtdExec = comp.compile(new StreamSource(baseDir + SCHEMA_DIRECTORY + File.separator + "odd2dtd.xsl"));
		XsltTransformer odd2dtdTransformer = odd2dtdExec.load();
		
		if(debug)
			odd2dtdTransformer.setParameter(new QName("verbose"), new XdmAtomicValue("true"));
		
		odd2dtdTransformer.setParameter(new QName("TEIC"), new XdmAtomicValue("true") );
		odd2dtdTransformer.setParameter(new QName("lang"), new XdmAtomicValue(language));
		odd2dtdTransformer.setParameter(new QName("doclang"), new XdmAtomicValue(language));
		odd2dtdTransformer.setParameter(new QName("parameterize"), new XdmAtomicValue(parameterizedDTD ? "true" : "false"));
		
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
		final ErrorHandlerImpl eh = new ErrorHandlerImpl();
		SchemaCollection sc =  inFormat.load(UriOrFile.toUri(input.getAbsolutePath()), inputParamArray, "rnc", eh);
		
		OutputDirectory od = new LocalOutputDirectory( 
				sc.getMainUri(),
				new File(outputDir + File.separator + schemaName + ".rnc"),
				"xsd",
				DEFAULT_OUTPUT_ENCODING,
                DEFAULT_LINE_LENGTH,
                DEFAULT_INDENT
        );
		
		of.output(sc, od, outputParamArray, "rng", eh);
	}
	
	public void generateXSD(File input) throws InputFailedException, InvalidParamsException, IOException, SAXException, OutputFailedException{
		InputFormat inFormat = new SAXParseInputFormat();
		OutputFormat of = new XsdOutputFormat();
		
		String[] inputParamArray = new String[]{};
		String[] outputParamArray = new String[]{"disable-abstract-elements"};
		final ErrorHandlerImpl eh = new ErrorHandlerImpl();
		SchemaCollection sc =  inFormat.load(UriOrFile.toUri(input.getAbsolutePath()), inputParamArray, "xsd", eh);
		
		OutputDirectory od = new LocalOutputDirectory( 
				sc.getMainUri(),
				new File(outputDir + File.separator + schemaName + ".xsd"),
				"xsd",
				DEFAULT_OUTPUT_ENCODING,
                DEFAULT_LINE_LENGTH,
                DEFAULT_INDENT
        );
		
		of.output(sc, od, outputParamArray, "rng", eh);
	}
	
	public void generateTEIDocumentation(XdmNode doc) throws SaxonApiException{
		// load stylesheets
		Processor proc = SaxonProcFactory.getProcessor();
		
		// prepare transformer
		XsltCompiler comp = proc.newXsltCompiler();
		XsltExecutable odd2teiDocExec = comp.compile(new StreamSource(baseDir + SCHEMA_DIRECTORY + File.separator + "odd2lite.xsl"));
		XsltTransformer odd2teiDocTransformer = odd2teiDocExec.load();
		
		odd2teiDocTransformer.setParameter(new QName("TEIC"), new XdmAtomicValue("true") );
		odd2teiDocTransformer.setParameter(new QName("localsource"), new XdmAtomicValue(P5SUBSET));
		odd2teiDocTransformer.setParameter(new QName("lang"), new XdmAtomicValue(language));
		odd2teiDocTransformer.setParameter(new QName("doclang"), new XdmAtomicValue(language));
		
		
		odd2teiDocTransformer.setInitialContextNode(doc);
		Serializer result = new Serializer();
		result.setOutputFile(new File(outputDocDir + File.separator + schemaName + ".xml") );
		odd2teiDocTransformer.setDestination(result);
		odd2teiDocTransformer.transform();
	}
	
	public void generateHTMLDocumentation(XdmNode doc) throws SaxonApiException, IOException{
		// load stylesheets
		Processor proc = SaxonProcFactory.getProcessor();
		
		// prepare transformer
		XsltCompiler comp = proc.newXsltCompiler();
		XsltExecutable odd2HTMLDocExec = comp.compile(new StreamSource(baseDir + SCHEMA_DIRECTORY + File.separator + "odd2html.xsl"));
		XsltTransformer odd2HTMLDocTransformer = odd2HTMLDocExec.load();
		
		odd2HTMLDocTransformer.setParameter(new QName("STDOUT"), new XdmAtomicValue("true") );
		odd2HTMLDocTransformer.setParameter(new QName("splitLevel"), new XdmAtomicValue("-1"));
		odd2HTMLDocTransformer.setParameter(new QName("lang"), new XdmAtomicValue(language));
		odd2HTMLDocTransformer.setParameter(new QName("doclang"), new XdmAtomicValue(language));
		odd2HTMLDocTransformer.setParameter(new QName("documentationLanguage"), new XdmAtomicValue(language));
		odd2HTMLDocTransformer.setParameter(new QName("cssFile"), new XdmAtomicValue("tei.css"));
		odd2HTMLDocTransformer.setParameter(new QName("cssSecondaryFile"), new XdmAtomicValue("odd.css"));
		
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


	public void setInformationArea(Text textInformation) {
		this.informationArea = textInformation;
	}
	
	
	

}
