package oucs.tei.roma;

import net.sf.saxon.s9api.Processor;

public class SaxonProcFactory {

	private static Processor processor;

	public static Processor getProcessor(){
		if(null == processor)
			processor = new Processor(false);
		
		return processor; 
	}
}
