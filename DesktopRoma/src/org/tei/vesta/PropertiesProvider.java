package org.tei.vesta;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.UnsupportedEncodingException;
import java.net.URLDecoder;
import java.util.InvalidPropertiesFormatException;
import java.util.Properties;

import org.tei.docx.DocXPropertiesProvider;
import org.tei.tei.TEIArchivePropertiesProvider;
import org.tei.tei.TEIPropertiesProvider;

public class PropertiesProvider 
	implements 
		DocXPropertiesProvider,
		TEIArchivePropertiesProvider,
		TEIPropertiesProvider{

	private static org.tei.vesta.PropertiesProvider instance;
	private String baseDir;
	private Properties properties;

	protected PropertiesProvider(){
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
		
		properties = new Properties();
		// load file
		try {
			properties.loadFromXML(Thread.currentThread().getContextClassLoader().getResourceAsStream("properties.xml"));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (InvalidPropertiesFormatException e) {
			e.printStackTrace();
		} catch (IOException e) {
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
	
	public String getP5Subset(){
		return baseDir + properties.getProperty("p5subset");
	}
	
	public String getStylesheetDir(){
		return baseDir + properties.getProperty("stylesheets.dir");
	}
	
	public String getCSSDir(){
		return baseDir + properties.getProperty("css.dir");
	}
	
	public String docx_pp_getDocXTemplateFile() {
		return baseDir + "../Stylesheets2/docx-templates/template.docx";
	}

	public String docx_pp_getStylesheetCheckDocx() {
		return baseDir + "../Stylesheets2/docx/tei-docx-conversion-vesta.xsl";
	}

	public String docx_pp_getStylesheetDocx2TEI() {
		return null;
	}

	public String docx_pp_getStylesheetNormalizeWordStyles() {
		return baseDir + "../Stylesheets2/docx/normalize-word-style.xsl";
	}

	public String docx_pp_getStylesheetTEI2Docx() {
		return baseDir + "../Stylesheets2/docx/tei-docx-conversion-vesta.xsl";
	}

	public String docx_pp_getTempDir() {
		return baseDir + "resources/tmp";
	}

	public String teiarc_pp_getTempDir() {
		return baseDir + "resources/tmp";
	}

	public String tei_pp_getStylesheetsDir() {
		return baseDir + "../Stylesheets2/odds2";
	}

	public String tei_pp_getP5Subset() {
		return baseDir + "resources/local/p5subset.xml";
	}

}
