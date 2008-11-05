package org.tei.docx;

/**
 * Properties needed for the DocX conversion.
 * 
 * @author Arno Mittelbach
 *
 */
public interface DocXPropertiesProvider {
	
	/**
	 * @return A temporary directory we can use.
	 */
	public String docx_pp_getTempDir();
	
	/**
	 * @return The docx to tei stylesheet to use.
	 */
	public String docx_pp_getStylesheetDocx2TEI();
	
	/**
	 * @return The nomarlize word styles stylesheet to use.
	 */
	public String docx_pp_getStylesheetNormalizeWordStyles();
	
	/**
	 * @return The tei to docx stylesheet to use.
	 */
	public String docx_pp_getStylesheetTEI2Docx();
	
	/**
	 * 
	 */
	public String docx_pp_getStylesheetCheckDocx();
	
	/**
	 * @return The word template file to use.
	 */
	public String docx_pp_getDocXTemplateFile();
	
	
	
}
