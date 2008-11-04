package org.tei.iso.converter;

import net.sf.saxon.s9api.Processor;

public class SaxonProcFactory {

	public static Processor getProcessor(){
		return oucs.tei.vesta.SaxonProcFactory.getProcessor();
	}
}
