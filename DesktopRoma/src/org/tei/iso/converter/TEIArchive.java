package org.tei.iso.converter;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

import net.sf.saxon.s9api.XdmNode;

public class TEIArchive {

	private String name;
	private String directoryName;

	public TEIArchive(String name, InputStream in){
		this.name = name;
		unzipData(in);
	}
	
	public TEIArchive(String name, File in){
		this.name = name;
		try {
			unzipData(new FileInputStream(in));
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}

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
	
	public XdmNode getTEIDocument(){
		return Utils.readFileIntoSaxonDoc(new File(directoryName + File.separator + "tei.xml"));
	}
	
	public String getDirectory(){
		return directoryName;
	}
	

	public void cleanUp() {
		// delete temporary dir
		Utils.deleteDirectory(new File(directoryName));
	}
}
