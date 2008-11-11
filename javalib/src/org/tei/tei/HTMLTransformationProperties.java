package org.tei.tei;

import java.io.File;

import javax.xml.transform.ErrorListener;

import net.sf.saxon.s9api.MessageListener;

abstract public class HTMLTransformationProperties {
	
	public ErrorListener getErrorListener() {
		return null;
	}

	public MessageListener getMessageListener() {
		return null;
	}
	
	public String getLanguage(){
		return "en";
	}
	
	public String getCSSFile(){
		return null;
	}

	public String getCSSSecondaryFile(){
		return null;
	}

	abstract public File getOutputFile();
}
