package org.tei.tei;

public interface TEIPropertiesProvider {
	/**
	 * @return The stylesheets dir (odds)
	 */
	public String tei_pp_getStylesheetsDir();
	
	/**
	 * 
	 * @return The p5 subset to use
	 */
	public String tei_pp_getP5Subset();
}
