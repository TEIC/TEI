package org.tei.genetic;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.xml.sax.ErrorHandler;
import org.xml.sax.SAXException;
import org.xml.sax.SAXParseException;

public class LoggingErrorHandler implements ErrorHandler {
	private static final Logger logger = Logger.getLogger(LoggingErrorHandler.class.getName());

	public void error(SAXParseException exception) throws SAXException {
		log(exception, Level.SEVERE);
	}

	public void fatalError(SAXParseException exception) throws SAXException {
		log(exception, Level.SEVERE);
	}

	public void warning(SAXParseException exception) throws SAXException {
		log(exception, Level.WARNING);
	}

	private void log(SAXParseException e, Level level) {
		logger.log(level, String.format("[line %d] %s", e.getLineNumber(), e.getMessage()));
	}
}
