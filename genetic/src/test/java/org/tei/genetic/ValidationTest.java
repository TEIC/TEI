package org.tei.genetic;

import java.io.File;
import java.io.FileFilter;
import java.io.IOException;
import java.util.logging.Logger;

import javax.xml.transform.TransformerFactory;
import javax.xml.transform.sax.SAXResult;
import javax.xml.transform.stream.StreamSource;

import org.junit.Assert;
import org.junit.Before;
import org.junit.Test;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;

import com.thaiopensource.util.PropertyMapBuilder;
import com.thaiopensource.validate.IncorrectSchemaException;
import com.thaiopensource.validate.Schema;
import com.thaiopensource.validate.ValidateProperty;
import com.thaiopensource.validate.Validator;
import com.thaiopensource.validate.rng.SAXSchemaReader;
import com.thaiopensource.xml.sax.Sax2XMLReaderCreator;

public class ValidationTest {
	private static final Logger logger = Logger.getLogger(ValidationTest.class.getName());
	private Schema schema;
	private TransformerFactory transformerFactory;
	private File validationDirectory;

	@Before
	public void initSchema() throws IOException, SAXException, IncorrectSchemaException {
		PropertyMapBuilder properties = new PropertyMapBuilder();
		properties.put(ValidateProperty.XML_READER_CREATOR, new Sax2XMLReaderCreator());
		properties.put(ValidateProperty.ERROR_HANDLER, new LoggingErrorHandler());
		schema = SAXSchemaReader.getInstance().createSchema(new InputSource(System.getProperty("genetic.schema")), properties.toPropertyMap());
		
		validationDirectory = new File(System.getProperty("genetic.validate.dir"));
		Assert.assertTrue(validationDirectory.isDirectory());
		
		transformerFactory = TransformerFactory.newInstance();
	}

	@Test
	public void validateTestDocuments() throws Exception {
		PropertyMapBuilder propertyMapBuilder = new PropertyMapBuilder();
		propertyMapBuilder.put(ValidateProperty.ERROR_HANDLER, new LoggingErrorHandler());
		Validator validator = schema.createValidator(propertyMapBuilder.toPropertyMap());
		File[] testFiles = validationDirectory.listFiles(new FileFilter() {

			public boolean accept(File pathname) {
				return pathname.isFile() && pathname.getName().endsWith(".xml");
			}
		});

		for (File testFile : testFiles) {
			logger.info(String.format("==================> %s", testFile.getName()));
			transformerFactory.newTransformer().transform(new StreamSource(testFile), new SAXResult(validator.getContentHandler()));
		}
	}
}
