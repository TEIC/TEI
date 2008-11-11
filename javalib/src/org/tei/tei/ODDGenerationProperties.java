package org.tei.tei;

import javax.xml.transform.ErrorListener;

import net.sf.saxon.s9api.MessageListener;

public abstract class ODDGenerationProperties {

	public ErrorListener getErrorListener() {
		return null;
	}

	public MessageListener getMessageListener() {
		return null;
	} 
	
	public boolean isVerbose(){
		return false;
	}
	
	public boolean isStripped(){
		return false;
	}
	
	public String getLanguage(){
		return "en";
	}
	
	public boolean isUseVersionFromTEI(){
		return true;
	}
	
	abstract public String getSchemaName();


}
