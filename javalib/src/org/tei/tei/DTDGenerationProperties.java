package org.tei.tei;

import java.io.File;

import javax.xml.transform.ErrorListener;

import net.sf.saxon.s9api.MessageListener;

public abstract class DTDGenerationProperties {
	public ErrorListener getErrorListener() {
		return null;
	}

	public MessageListener getMessageListener() {
		return null;
	} 
	
	public boolean isVerbose(){
		return false;
	}
	
	public String getLanguage(){
		return "en";
	}
	
	public boolean isParameterizedDTD(){
		return false;
	}
	
	abstract public File getOutputFile();
	
	abstract public String getSchemaName();


}
