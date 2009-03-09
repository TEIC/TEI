package org.tei.tei;

import java.io.File;

import javax.xml.transform.ErrorListener;

import net.sf.saxon.s9api.MessageListener;
import net.sf.saxon.s9api.XsltTransformer;

abstract public class TransformationProperties {

	
	public ErrorListener getErrorListener() {
		return null;
	}

	public MessageListener getMessageListener() {
		return null;
	}

	abstract public File getOutputFile();
	
	public String getProfile(){
		return "default";
	}
	
	abstract public String getFormat();

	
	public void setStylesheetParameters(XsltTransformer transformer){
	}
}
