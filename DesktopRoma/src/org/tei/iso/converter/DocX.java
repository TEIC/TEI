package org.tei.iso.converter;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;

import javax.print.attribute.standard.Destination;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamSource;

import net.sf.saxon.s9api.Processor;
import net.sf.saxon.s9api.QName;
import net.sf.saxon.s9api.SaxonApiException;
import net.sf.saxon.s9api.Serializer;
import net.sf.saxon.s9api.XdmAtomicValue;
import net.sf.saxon.s9api.XdmDestination;
import net.sf.saxon.s9api.XdmNode;
import net.sf.saxon.s9api.XsltCompiler;
import net.sf.saxon.s9api.XsltExecutable;
import net.sf.saxon.s9api.XsltTransformer;

public class DocX {

	private String directoryName; 
	
	private String name;
	
	/**
	 * Store the metadata document, that we should use, when creating the TEI
	 */
	private File metadata;
	
	/**
	 * contains the document to the corresponding tei document
	 */
	private XdmNode teiDocumentNode;
	
	/**
	 * Defines which directories are copied to and from the archive
	 */
	private String[] archiveDirectoriesToCopy = new String[]{
		 "word/media",
		 "word/embeddings",
		 "word/fonts"
	};
	
	private String[] archiveFilesToCopy = new String[]{
		 "[Content_Types].xml"
	};
	
	private File zipFile;
	
	private File teiArchive;
	
	/**
	 * Constructs a docx object from some docx inputstream
	 * @param name
	 * @param in
	 * @param metadata
	 */
	public DocX(String name, InputStream in, File metadata){
		this.name = name;
		this.metadata = metadata;
		unzipData(in);
	}
	
	/**
	 * Constructs a docx object from the empty template
	 * @param teiDoc
	 */
	public DocX(String name, File metadata){
		this.name = name;
		this.metadata = metadata;
		
		// copy template somewhere
		File templateFile = new File(PropertiesProvider.getInstance().getDocXTemplateFile());
		try {
			InputStream in = new FileInputStream(templateFile);
			unzipData(in);
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
	
	public DocX(String name, TEIArchive archive, File metadata){
		this(name, metadata);

		// copy directories from archive in docx file 
		for(String dirName : archiveDirectoriesToCopy){
			File dir = new File(archive.getDirectory() + File.separator + dirName);
			if(dir.exists() && dir.isDirectory()){
				try {
					Utils.copyDir(dir, new File(directoryName + File.separator + dirName));
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		
		// copy files
		for(String fileName : archiveFilesToCopy){
			File file = new File(archive.getDirectory() + File.separator + fileName);
			if(file.exists()){
				try {
					Utils.copyFile(file, new File(directoryName + File.separator + fileName));
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
	}
	
	
	
	
	/**
	 * Unzips the .docx file
	 * @param in
	 */
	private void unzipData(InputStream in) {
		// where should we unzip the file to
		String tmpDir = PropertiesProvider.getInstance().getTempDir();
		// name of the directory
		directoryName = tmpDir + File.separator + UUID.randomUUID().toString();
		// buffer
		int BUFFER = 2048;
		// output
		BufferedOutputStream dest;
		
		// create tmp directory
		new File(directoryName).mkdir();
		
		// read file
		ZipInputStream zis = new ZipInputStream(new BufferedInputStream(in));
		
		// decompress file
		ZipEntry entry;
		try {
			while((entry = zis.getNextEntry()) != null) {
				// if it is a directory, create it
				if(entry.isDirectory()){
					File dir = new File(directoryName + File.separator + entry.getName()); 
					if(! dir.exists())
						dir.mkdirs();
					continue;
				}
				
				// create directories if necessary
				new File( new File(directoryName + File.separator + entry.getName()).getParent() ).mkdirs();
				
				
				int count;
				byte data[] = new byte[BUFFER];
	            // write the files to the disk
				FileOutputStream fos = new FileOutputStream(directoryName + File.separator + entry.getName());
	            dest = new BufferedOutputStream(fos, BUFFER);
	            while ((count = zis.read(data, 0, BUFFER)) != -1) {
	               dest.write(data, 0, count);
	            }
	            dest.flush();
	            dest.close();
			}
		} catch (IOException e) {
			e.printStackTrace();
		}		
	}

	/**
	 * Transforms this Word document to TEI.
	 * @return The TEI version of this .docx
	 */
	public XdmNode getTEI(){
		if(teiDocumentNode instanceof XdmNode)
			return teiDocumentNode;
		
		/* create teiDoc */
		try {
			XdmNode doc = Utils.readFileIntoSaxonDoc(new File(directoryName + File.separator + "word" + File.separator + "document.xml"));
			
			// load stylesheets
			Processor proc = SaxonProcFactory.getProcessor();
			
			XsltCompiler comp = proc.newXsltCompiler();
			XsltExecutable normalizerExec = comp.compile(new StreamSource(new File(PropertiesProvider.getInstance().getStylesheetNormalizeWordStyles())));
			XsltExecutable docx2teiExec = comp.compile(new StreamSource(new File(PropertiesProvider.getInstance().getStylesheetDocx2TEI())));
			XsltTransformer normalizer = normalizerExec.load();
			XsltTransformer docx2tei = docx2teiExec.load();
			
			// set directory
			normalizer.setParameter(new QName("word-directory"), new XdmAtomicValue(directoryName));
			docx2tei.setParameter(new QName("word-directory"), new XdmAtomicValue(directoryName));

			// set whether to use the metadata document or not
			if(null != metadata){
				docx2tei.setParameter(new QName("metadata-file"), new XdmAtomicValue(metadata.getAbsolutePath()));
			}
			
			// transform part1
			normalizer.setInitialContextNode(doc);
			XdmDestination tmpDest = new XdmDestination();
			normalizer.setDestination(tmpDest);
			normalizer.transform();
			
			// transform part2
			XdmDestination result = new XdmDestination();
			docx2tei.setInitialContextNode(tmpDest.getXdmNode());
			docx2tei.setDestination(result);
			docx2tei.transform();
			
			// store node
			teiDocumentNode = result.getXdmNode();
			
		} catch (SaxonApiException e) {
			// TODO Auto-generated catch block
			e.printStackTrace();
		}
		
		return teiDocumentNode;
	}
	
	public File getTEIArchive(){
		if(null != teiArchive)
			return teiArchive;
		
		// create temporary directory
		String tmpDir = PropertiesProvider.getInstance().getTempDir();
		String tmpArchiveDirName = tmpDir + File.separator + UUID.randomUUID().toString();
		File tmpArchiveDir = new File(tmpArchiveDirName);
		tmpArchiveDir.mkdir();
		
		// get teiDoc and store it in directory
		XdmNode teiDocument = getTEI();
		Utils.storeSaxonDoc(teiDocument, new File(tmpArchiveDirName + File.separator + "tei.xml"));

		// copy directories
		for(String dirName : archiveDirectoriesToCopy){
			File dir = new File(directoryName + File.separator + dirName);
			if(dir.exists() && dir.isDirectory()){
				// try to create necessary directories
				if(dirName.indexOf('/') != -1 && !dirName.substring(0,dirName.lastIndexOf('/')).equals("")){
					File dirToCreate = new File(tmpArchiveDirName + File.separator + dirName.substring(0,dirName.lastIndexOf('/')));
					if(!dirToCreate.isDirectory())
						dirToCreate.mkdirs();
				}
				
				// copy directory
				try {
					Utils.copyDir(dir, new File(tmpArchiveDirName + File.separator + dirName));
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		
		// copy files
		for(String fileName : archiveFilesToCopy){
			File file = new File(directoryName + File.separator + fileName);
			if(file.exists()){
				// try to create necessary directories
				if(fileName.indexOf('/') != -1 && !fileName.substring(0,fileName.lastIndexOf('/')).equals("")){
					File dirToCreate = new File(tmpArchiveDirName + File.separator + fileName.substring(0,fileName.lastIndexOf('/')));
					if(!dirToCreate.isDirectory())
						dirToCreate.mkdirs();
				}
				
				// copy file
				try {
					Utils.copyFile(file, new File(tmpArchiveDirName + File.separator + fileName));
				} catch (IOException e) {
					e.printStackTrace();
				}
			}
		}
		
		// create zipfile
		try {
			teiArchive = new File(tmpDir + File.separator + UUID.randomUUID().toString() + ".zip");
			FileOutputStream dest = new FileOutputStream(teiArchive);
			ZipOutputStream out = new ZipOutputStream(new BufferedOutputStream(dest));
			   		
			// gather files and zip them
			constructZip(new File(tmpArchiveDirName), out, "");
			
            out.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		// delete directory
		Utils.deleteDirectory(tmpArchiveDir);
		
		return teiArchive;
	}
	

	public void mergeTEI(XdmNode teiDoc) {
		mergeTEI(teiDoc, null);
	}
	
	/**
	 * Merges a TEI document back into a docx
	 * @param teiDoc
	 */
	public void mergeTEI(XdmNode teiDoc, String template) {
		// load stylesheets
		TransformerFactory transFact = TransformerFactory.newInstance();
		try {
			// prepare transformation
			Processor proc = SaxonProcFactory.getProcessor();
			XsltCompiler comp = proc.newXsltCompiler();
			XsltExecutable toDocXExec = comp.compile(new StreamSource(new File(PropertiesProvider.getInstance().getStylesheetTEI2Docx())));
			XsltTransformer toDocX = toDocXExec.load();
			
			toDocX.setParameter(new QName("word-directory"), new XdmAtomicValue(directoryName));

			if(null != template)
				toDocX.setParameter(new QName("template"), new XdmAtomicValue(template));
			
			// move [Content_Types].xml to Content_Types.xml
			File orgConTypesFile = new File(directoryName + File.separator + "[Content_Types].xml");
			File tmpConTypesFile = new File(directoryName + File.separator + "Content_Types.xml");
			orgConTypesFile.renameTo(tmpConTypesFile);
			
			// transform and write back to document.xml
			File wordDotXMLFile = new File(directoryName + File.separator + "word" + File.separator + "document.xml");
			Serializer result = new Serializer();
			result.setOutputFile(wordDotXMLFile);
			toDocX.setInitialContextNode(teiDoc);
			toDocX.setDestination(result);
			toDocX.transform();
			
			
			// remove original core.xml file
			File orgCoreFile = new File(directoryName + File.separator + "docProps" + File.separator + "core.xml");
			orgCoreFile.delete();
			
			// move new core.xml
			File newCoreFile = new File(directoryName + File.separator + "docProps" + File.separator + "newcore.xml");
			newCoreFile.renameTo(orgCoreFile);
			
			// remove Content_Types.xml
			tmpConTypesFile.delete();
			
			// move new ContentTypes 
			File newConTypesFile = new File(directoryName + File.separator + "newContent_Types.xml");
			newConTypesFile.renameTo(orgConTypesFile);
		} catch (SaxonApiException e) {
			e.printStackTrace();
		}
	}


	
	public File getDocXFile(){
		
		// zip file
		String tmpDir = PropertiesProvider.getInstance().getTempDir();
		zipFile = null;
		try {
			zipFile = new File(tmpDir + File.separator + UUID.randomUUID().toString() + ".zip");
			FileOutputStream dest = new FileOutputStream(zipFile);
			ZipOutputStream out = new ZipOutputStream(new BufferedOutputStream(dest));
			   		
			// gather files and zip them
			constructZip(new File(directoryName), out, "");
			
            out.close();
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		} catch (IOException e) {
			e.printStackTrace();
		}

		return zipFile;
	}
	
	private void constructZip(File file, ZipOutputStream out, String dir) throws IOException{
		BufferedInputStream origin = null;
		int BUFFER = 2048;
		byte data[] = new byte[BUFFER];
		
		File[] files = file.listFiles();
		for (int i=0; i<files.length; i++) {
               if(files[i].isDirectory()){
            	   constructZip(files[i], out, dir + files[i].getName() + File.separator);
            	   continue;
               }
				
			   // read file
			   FileInputStream fi = new FileInputStream(files[i]);
			   origin = new  BufferedInputStream(fi, BUFFER);

			   // create zip entry
			   ZipEntry entry = new ZipEntry(dir + files[i].getName());
			   
			   // add entries to ZIP file
			   out.putNextEntry(entry);
			   
			   // write data
			   int count;
			   while((count = origin.read(data, 0, BUFFER)) != -1) {
			      out.write(data, 0, count);
			   }
			   
			   // close 
			   origin.close();
		}
	}

	/**
	 * 
	 * @return This .docx's name
	 */
	public String getName() {
		return name;
	}


	public void cleanUp() {
		// delete temporary dir
		Utils.deleteDirectory(new File(directoryName));
		
		// delete zip file
		if(null != zipFile && zipFile.exists())
			zipFile.delete();
		
		// delete archive
		if(null != teiArchive && teiArchive.exists())
			teiArchive.delete();
	}

	
}
