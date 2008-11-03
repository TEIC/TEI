package oucs.tei.vesta;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
import java.io.UnsupportedEncodingException;
import java.util.Iterator;

import javax.xml.namespace.NamespaceContext;
import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.TransformerFactoryConfigurationError;
import javax.xml.transform.dom.DOMSource;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.SaxonApiException;
import net.sf.saxon.s9api.XdmNode;

import org.w3c.dom.Document;
import org.xml.sax.SAXException;


public class Utils {
	public static XdmNode readInputStreamIntoSaxonDoc(InputStream in){
		Processor proc = SaxonProcFactory.getProcessor();
		net.sf.saxon.s9api.DocumentBuilder builder = proc.newDocumentBuilder();
		
		try {
			return builder.build(new StreamSource(in));
		} catch (SaxonApiException e) {
			e.printStackTrace();
		}
		return null;
	}
	
	public static XdmNode readFileIntoSaxonDoc(File file) throws SaxonApiException{
		Processor proc = SaxonProcFactory.getProcessor();
		net.sf.saxon.s9api.DocumentBuilder builder = proc.newDocumentBuilder();
		
		return builder.build(file);
	}
	
	public static void storeSaxonDoc(XdmNode node, File file){
		OutputStreamWriter out;
		try {
			
			out = new OutputStreamWriter(new FileOutputStream(file),"UTF-8");
			String document = Utils.getXMLNodeAsString(node);
			out.write(document);
			out.flush();
			out.close();
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
	}
	public static String getXMLNodeAsString(XdmNode node){
		StringWriter writer = new StringWriter();
		
		Transformer serializer;
		try {
			serializer = TransformerFactory.newInstance().newTransformer();

			serializer.setOutputProperty(OutputKeys.METHOD, "xml");
			serializer.setOutputProperty(OutputKeys.ENCODING, "utf-8");
			serializer.setOutputProperty(OutputKeys.INDENT, "yes");
			serializer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
			
			serializer.transform(node.asSource(), new StreamResult(writer));
		} catch (TransformerConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerFactoryConfigurationError e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		} catch (TransformerException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return writer.toString();
	}	
	
	public static Document readInputFileAsXML(File in) throws ParserConfigurationException, SAXException, IOException {
		DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        docBuilderFactory.setNamespaceAware(true);
        docBuilderFactory.setXIncludeAware(true);
		DocumentBuilder docBuilder;
        Document doc = null;
       
		docBuilder = docBuilderFactory.newDocumentBuilder();
		doc = docBuilder.parse(in);
	    
        return doc;
	}
	
	public static void writeDomDocumentToFile(Document document, File file){
		OutputStreamWriter out = null;
		try {
			out = new OutputStreamWriter(new FileOutputStream(file),"UTF-8");
		} catch (IOException e1) {
			e1.printStackTrace();
		}
		
		Transformer serializer;

		try {
			serializer = TransformerFactory.newInstance().newTransformer();
			serializer.setOutputProperty(OutputKeys.METHOD, "xml");
			serializer.setOutputProperty(OutputKeys.ENCODING, "utf-8");
			serializer.setOutputProperty(OutputKeys.INDENT, "yes");
			serializer.setOutputProperty(OutputKeys.STANDALONE, "yes");
			
			serializer.transform(new DOMSource(document), new StreamResult(out));
		} catch (TransformerConfigurationException e) {
			e.printStackTrace();
		} catch (TransformerFactoryConfigurationError e) {
			e.printStackTrace();
		} catch (TransformerException e) {
			e.printStackTrace();
		}
	}
	
    public static void copyFile(File src, File dest) throws IOException{
		  if(src.exists()){
			  InputStream in = new FileInputStream(src);
			  OutputStream out = new FileOutputStream(dest); 
			  // Transfer bytes from in to out
			  byte[] buf = new byte[1024];
			  int len;
			  while ((len = in.read(buf)) > 0) {
				  out.write(buf, 0, len);
			  }
			  in.close();
			  out.close();
		  }
	}
    
	public static NamespaceContext getTEINamespaceContext(){
		NamespaceContext ctx = new NamespaceContext() {
	        public String getNamespaceURI(String prefix) {
	            String uri;
	            if (prefix.equals("tei"))
	                uri = "http://www.tei-c.org/ns/1.0";
	            else
	                uri = null;
	            
	            return uri ;
	        }
	       
	        // Dummy implementation - not used!
	        public Iterator<String> getPrefixes(String val) {
	            return null;
	        }
	       
	        // Dummy implemenation - not used!
	        public String getPrefix(String uri) {
	            return null;
	        }
	    };
	    
	    return ctx;
	}
}
