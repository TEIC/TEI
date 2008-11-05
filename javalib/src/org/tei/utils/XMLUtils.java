package org.tei.utils;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.StringWriter;
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
import org.w3c.dom.Node;
import org.xml.sax.SAXException;

/**
 * Provides various helper functions to work with XML.
 * 
 * @author Arno Mittelbach
 *
 */
public class XMLUtils {
	
	/**
	 * Stores a Saxon XML document.
	 * 
	 * @param node The saxon document.
	 * @param file The desitination.
	 * @throws IOException
	 */
	public static void storeDocument(XdmNode node, File file) throws IOException{
		OutputStreamWriter out;

		out = new OutputStreamWriter(new FileOutputStream(file),"UTF-8");
		String document = getXMLNodeAsString(node);
		out.write(document);
		out.flush();
		out.close();
	}
	
	/**
	 * Reads a file into a Saxon XML document.
	 * 
	 * @param file The input file.
	 * @return The Saxon XML document.
	 * @throws SaxonApiException
	 */
	public static XdmNode readFileIntoSaxonDoc(File file) throws SaxonApiException{
		Processor proc = SaxonProcFactory.getProcessor();
		net.sf.saxon.s9api.DocumentBuilder builder = proc.newDocumentBuilder();

		return builder.build(file);
	}
	
	/**
	 * Reads an InputStream into a Saxon XML document.
	 * 
	 * @param in An InputStream providing the XML data.
	 * @return The Saxon XML document.
	 * @throws SaxonApiException
	 */
	public static XdmNode readInputStreamIntoSaxonDoc(InputStream in) throws SaxonApiException{
		Processor proc = SaxonProcFactory.getProcessor();
		net.sf.saxon.s9api.DocumentBuilder builder = proc.newDocumentBuilder();

		return builder.build(new StreamSource(in));
	}
	
	/**
	 * Reads a file into a JAXP XML document.
	 * 
	 * @param file The input file.
	 * @return The JAXP XML document.
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws IOException
	 */
	public static Document readInputFileIntoJAXPDoc(File file) throws ParserConfigurationException, SAXException, IOException {
		DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        docBuilderFactory.setNamespaceAware(true);
		DocumentBuilder docBuilder;
        Document doc = null;
       
		docBuilder = docBuilderFactory.newDocumentBuilder();
		doc = docBuilder.parse(file);
	    
        return doc;
	}
	
	/**
	 * Reads an InputStream into a JAXP XML document.
	 * 
	 * @param in The input stream.
	 * @return The JAXP XML document.
	 * @throws ParserConfigurationException
	 * @throws SAXException
	 * @throws IOException
	 */
	public static Document readInputStreamIntoJAXPDoc(InputStream in) throws ParserConfigurationException, SAXException, IOException {
		DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        docBuilderFactory.setNamespaceAware(true);
		DocumentBuilder docBuilder;
        Document doc = null;
       
		docBuilder = docBuilderFactory.newDocumentBuilder();
		doc = docBuilder.parse(in);
	    
        return doc;
	}
	
	/**
	 * Creates an empty JAXP XML document.
	 * 
	 * @return An empty JAXP XML document.
	 */
	public static Document getNewEmptyJAXPDoc(){
		DocumentBuilderFactory documentBuilderFactory =	DocumentBuilderFactory.newInstance();
		documentBuilderFactory.setNamespaceAware(true);
		DocumentBuilder documentBuilder;

		try {
			documentBuilder = documentBuilderFactory.newDocumentBuilder();
			return documentBuilder.newDocument();
		} catch (ParserConfigurationException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	/**
	 * Transforms an XML node (from a JAXP XML document) into a formatted string.
	 * 
	 * @param node The node to transform.
	 * @return A string representation of the node. 
	 */
	public static String getXMLNodeAsString(Node node){
		StringWriter writer = new StringWriter();
		
		Transformer serializer;

		try {
			serializer = TransformerFactory.newInstance().newTransformer();
			serializer.setOutputProperty(OutputKeys.METHOD, "xml");
			serializer.setOutputProperty(OutputKeys.ENCODING, "utf-8");
			serializer.setOutputProperty(OutputKeys.INDENT, "yes");
			serializer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes");
			serializer.transform(new DOMSource(node), new StreamResult(writer));
		} catch (TransformerConfigurationException e) {
			e.printStackTrace();
		} catch (TransformerFactoryConfigurationError e) {
			e.printStackTrace();
		} catch (TransformerException e) {
			e.printStackTrace();
		}
		
		return writer.toString();
	}
	
	/**
	 * Transforms an XML node (from a SAXON XML document) into a formatted string.
	 * 
	 * @param node The node to transform.
	 * @return A string representation of the node. 
	 */
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
			e.printStackTrace();
		} catch (TransformerFactoryConfigurationError e) {
			e.printStackTrace();
		} catch (TransformerException e) {
			e.printStackTrace();
		}
		
		return writer.toString();
	}
	
	/**
	 * Creates a NamespaceContext object with the following list of namespaces:
	 * 
	 * <dl>
	 *  <dt>tei</dt>
	 *  <dd>http://www.tei-c.org/ns/1.0</dd>
	 * </dl>
	 * 
	 * @return A NamespaceContext object
	 */
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
