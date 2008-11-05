package org.tei.utils;

import net.sf.saxon.FeatureKeys;
import net.sf.saxon.s9api.Processor;

/**
 * Provides access to a Saxon Processor.
 * 
 * @author Arno Mittelbach
 *
 */
public class SaxonProcFactory {

	private static Processor processor;

	/**
	 * Stores and reuses a Saxon processor.
	 * @return The Saxon processor.
	 */
	public static Processor getProcessor(){
		if(null == processor){
			processor = new Processor(false);
			processor.setConfigurationProperty(FeatureKeys.XINCLUDE, true);
		}
		return processor; 
	}
}
