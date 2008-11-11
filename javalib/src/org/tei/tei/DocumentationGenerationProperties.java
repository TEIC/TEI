package org.tei.tei;

import javax.xml.transform.ErrorListener;

import net.sf.saxon.s9api.MessageListener;

public abstract class DocumentationGenerationProperties {
	public ErrorListener getErrorListener() {
		return null;
	}

	public MessageListener getMessageListener() {
		return null;
	} 
	
	public String getLanguage(){
		return "en";
	}
	
}
