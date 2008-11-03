package oucs.tei.vesta;

import net.sf.saxon.FeatureKeys;
import net.sf.saxon.s9api.Processor;

public class SaxonProcFactory {

	private static Processor processor;

	public static Processor getProcessor(){
		if(null == processor){
			processor = new Processor(false);
			processor.setConfigurationProperty(FeatureKeys.XINCLUDE, true);
		}
		return processor; 
	}
}
