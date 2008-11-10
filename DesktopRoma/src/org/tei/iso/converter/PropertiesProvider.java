package org.tei.iso.converter;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.InvalidPropertiesFormatException;
import java.util.Properties;

import org.tei.vesta.VestaProcessor;


/**
 * Provides access to various properties.
 * 
 * This file is implemented as a singleto.
 * @author Arno Mittelbach
 *
 */
public class PropertiesProvider {

	/**
	 * Stores the location of the properties file
	 */
	private final String propertiesFile = "properties.xml";
	
	
	/**
	 * Stores the instance to this singleton.
	 */
	private static PropertiesProvider instance;
	
	/**
	 * Stores the properties object
	 */
	private Properties properties;

	private String baseDir;
	
	/**
	 * Reduce access to this type to protected and construct object.
	 */
	protected PropertiesProvider(){
		properties = new Properties();
		// load file
		try {
			properties.loadFromXML(Thread.currentThread().getContextClassLoader().getResourceAsStream(propertiesFile));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (InvalidPropertiesFormatException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}
		
		baseDir = VestaProcessor.class.getProtectionDomain().getCodeSource().getLocation().getPath();
		baseDir = baseDir.substring(0, baseDir.lastIndexOf(File.separator));
		File baseDirFile = new File(baseDir + File.separator + "resources");
		if(!baseDirFile.exists())
			baseDir = baseDir.substring(0, baseDir.lastIndexOf(File.separator));
		baseDir += File.separator;
		
		try {
			baseDir = URLDecoder.decode(baseDir,"UTF-8");
		} catch (UnsupportedEncodingException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Provides access to the type PropertiesProvider.
	 * @return The instance of PropertiesProvider.
	 */
	public static PropertiesProvider getInstance(){
		if(null == instance)
			instance = new PropertiesProvider();
		
		return instance;
	}
	
	/**
	 * 
	 * @return A temporary directory we can use
	 */
	public String getTempDir(){
		return baseDir + File.separator + properties.getProperty("directories.tmp");
	}
	
	public String getStylesheetDocx2TEI(){
		return baseDir + File.separator + properties.getProperty("stylesheets.docx-tei"); 
	}
	
	public String getStylesheetNormalizeWordStyles(){
		return baseDir + File.separator + properties.getProperty("stylesheets.normalize-word-style"); 
	}
	
	public String getDocXTemplateFile(){
		return baseDir + File.separator + properties.getProperty("templates.docx");
	}
	
	
	public String getStylesheetTEI2Docx(){
		return baseDir + File.separator + properties.getProperty("stylesheets.tei-docx");
	}

}
