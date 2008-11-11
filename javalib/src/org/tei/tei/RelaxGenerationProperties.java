package org.tei.tei;

import javax.xml.transform.ErrorListener;

import net.sf.saxon.s9api.MessageListener;

public abstract class RelaxGenerationProperties {

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
	
	public String getPatternPrefix(){
		return "_tei";
	}
	
	
	abstract public String getSchemaName();

}
