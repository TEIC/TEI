package org.tei.iso.converter;

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

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;
import javax.xml.parsers.ParserConfigurationException;
import javax.xml.transform.OutputKeys;
import javax.xml.transform.Source;
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

public class Utils {

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
	
	public static XdmNode readFileIntoSaxonDoc(File file){
		Processor proc = SaxonProcFactory.getProcessor();
		net.sf.saxon.s9api.DocumentBuilder builder = proc.newDocumentBuilder();
		try {
			return builder.build(file);
		} catch (SaxonApiException e) {
			e.printStackTrace();
		}
		return null;
	}
	
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
	
	public static Document readInputFileAsXML(File file) throws ParserConfigurationException, SAXException, IOException {
		DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        docBuilderFactory.setNamespaceAware(true);
		DocumentBuilder docBuilder;
        Document doc = null;
       
		docBuilder = docBuilderFactory.newDocumentBuilder();
		doc = docBuilder.parse(file);
	    
        return doc;
	}
	
	public static Document readInputStreamAsXML(InputStream in) throws ParserConfigurationException, SAXException, IOException {
		DocumentBuilderFactory docBuilderFactory = DocumentBuilderFactory.newInstance();
        docBuilderFactory.setNamespaceAware(true);
		DocumentBuilder docBuilder;
        Document doc = null;
       
		docBuilder = docBuilderFactory.newDocumentBuilder();
		doc = docBuilder.parse(in);
	    
        return doc;
	}
	
	public static Document getNewEmptyDocument(){
		DocumentBuilderFactory documentBuilderFactory =	DocumentBuilderFactory.newInstance();
		documentBuilderFactory.setNamespaceAware(true);
		DocumentBuilder documentBuilder;
		try {
			documentBuilder = documentBuilderFactory.newDocumentBuilder();
			return documentBuilder.newDocument();
		} catch (ParserConfigurationException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return null;
	}
	
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
	
	  public static void copyDir(File src, File dest) throws IOException{
		  if (src.isDirectory()){
			  if (!dest.exists())
				  dest.mkdir();
			  
			  String files[] = src.list();

			  for(int i = 0; i < files.length; i++)
				  copyDir(new File(src, files[i]), new File(dest, files[i]));
		  } else {
			  copyFile(src, dest);
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
	  
		
	  public static  boolean deleteDirectory(File dir) {
	        if (dir.isDirectory())
	            for (String child : dir.list()) 
	                if (! deleteDirectory(new File(dir, child))) 
	                    return false;
	    
	        return dir.delete();
	  }

}
