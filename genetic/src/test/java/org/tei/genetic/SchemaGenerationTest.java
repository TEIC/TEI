package org.tei.genetic;

import java.io.File;

import javax.xml.transform.Source;
import javax.xml.transform.stream.StreamSource;

import net.sf.saxon.FeatureKeys;
import net.sf.saxon.s9api.Destination;
import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.QName;
import net.sf.saxon.s9api.SaxonApiException;
import net.sf.saxon.s9api.Serializer;
import net.sf.saxon.s9api.XdmAtomicValue;
import net.sf.saxon.s9api.XsltCompiler;
import net.sf.saxon.s9api.XsltTransformer;

import org.junit.Test;

public class SchemaGenerationTest {
	private static final File STYLESHEETS = new File("../Stylesheets");
	private static final File P5_SUBSET = new File("../P5/p5subset.xml");
	private static final File ODD = new File("geneticTEI.xml");
	private static final String SCHEMA_NAME = "geneticTEI";
	private static final String LANGUAGE = "en";
	private static final Boolean VERBOSE = false;
	private static final File RNG_SCHEMA_TARGET = new File("geneticTEI.rng");

	@Test
	public void generateSchema() throws Exception {
		Serializer serializer = new Serializer();
		serializer.setOutputFile(RNG_SCHEMA_TARGET);

		try {
			Processor processor = new Processor(false);
			processor.setConfigurationProperty(FeatureKeys.XINCLUDE, true);
			XsltCompiler xsltCompiler = processor.newXsltCompiler();

			XsltTransformer odd2relax = xsltCompiler.compile(new StreamSource(new File(STYLESHEETS, "odds2/odd2relax.xsl"))).load();
			odd2relax.setDestination(serializer);
			odd2relax.setParameter(new QName("verbose"), new XdmAtomicValue(VERBOSE.toString()));
			odd2relax.setParameter(new QName("TEIC"), new XdmAtomicValue("true"));
			odd2relax.setParameter(new QName("lang"), new XdmAtomicValue(LANGUAGE));
			odd2relax.setParameter(new QName("doclang"), new XdmAtomicValue(LANGUAGE));
			odd2relax.setParameter(new QName("parameterize"), new XdmAtomicValue("false"));
			odd2relax.setParameter(new QName("patternPrefix"), new XdmAtomicValue("_tei"));

			buildCompiler(xsltCompiler, new StreamSource(ODD), odd2relax).transform();
		} finally {
			serializer.close();
		}
	}

	@Test
	public void generateDocumentation() throws Exception {
		Serializer serializer = new Serializer();
		serializer.setOutputFile(new File("geneticTEI.html"));
		
		try {
			Processor processor = new Processor(false);
			processor.setConfigurationProperty(FeatureKeys.XINCLUDE, true);
			XsltCompiler xsltCompiler = processor.newXsltCompiler();

			XsltTransformer odd2doc = xsltCompiler.compile(new StreamSource(new File(STYLESHEETS, "odds2/odd2html.xsl"))).load();
			odd2doc.setDestination(serializer);
			odd2doc.setParameter(new QName("TEIC"), new XdmAtomicValue("true"));
			odd2doc.setParameter(new QName("localsource"), new XdmAtomicValue(P5_SUBSET.toURI()));
			odd2doc.setParameter(new QName("lang"), new XdmAtomicValue(LANGUAGE));
			odd2doc.setParameter(new QName("doclang"), new XdmAtomicValue(LANGUAGE));
			odd2doc.setParameter(new QName("splitLevel"), new XdmAtomicValue("-1"));
			odd2doc.setParameter(new QName("STDOUT"), new XdmAtomicValue("true"));

			buildCompiler(xsltCompiler, new StreamSource(ODD), odd2doc).transform();
		} finally {
			serializer.close();
		}
	}

	protected XsltTransformer buildCompiler(XsltCompiler xsltCompiler, Source odd, Destination result) throws SaxonApiException {
		XsltTransformer odd2odd = xsltCompiler.compile(new StreamSource(new File(STYLESHEETS, "odds2/odd2odd.xsl"))).load();

		odd2odd.setParameter(new QName("selectedSchema"), new XdmAtomicValue(SCHEMA_NAME));
		odd2odd.setParameter(new QName("verbose"), new XdmAtomicValue(VERBOSE.toString()));
		odd2odd.setParameter(new QName("stripped"), new XdmAtomicValue("true"));
		odd2odd.setParameter(new QName("localsource"), new XdmAtomicValue(P5_SUBSET.toURI()));
		odd2odd.setParameter(new QName("TEIC"), new XdmAtomicValue("true"));
		odd2odd.setParameter(new QName("lang"), new XdmAtomicValue(LANGUAGE));
		odd2odd.setParameter(new QName("doclang"), new XdmAtomicValue(LANGUAGE));
		odd2odd.setParameter(new QName("useVersionFromTEI"), new XdmAtomicValue("true"));

		odd2odd.setSource(odd);
		odd2odd.setDestination(result);
		return odd2odd;
	}
}
