package org.tei.iso.converter;

import net.sf.saxon.s9api.Processor;

public class SaxonProcFactory {

	public static Processor getProcessor(){
		return org.tei.vesta.SaxonProcFactory.getProcessor();
	}
}
