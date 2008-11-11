package org.tei.tei;

import java.io.File;

import javax.xml.transform.ErrorListener;

import org.tei.docx.DocXPropertiesProvider;

import net.sf.saxon.s9api.MessageListener;

public abstract class DocXTransformationProperties implements DocXPropertiesProvider {
	
	public ErrorListener getErrorListener() {
		return null;
	}

	public MessageListener getMessageListener() {
		return null;
	} 
	
	abstract public File getOutputFile();
}
