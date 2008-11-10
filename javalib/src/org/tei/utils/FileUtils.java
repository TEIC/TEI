package org.tei.utils;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.UUID;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;
import java.util.zip.ZipOutputStream;


/**
 * Provides helper methods for working with files.
 * 
 * @author Arno Mittelbach
 *
 */
public class FileUtils {

	/**
	 * Copies a file.
	 * 
	 * @param src The source.
	 * @param dest The destination.
	 * @throws IOException
	 */
	public static void copyFile(File src, File dest) throws IOException{
		  if(src.exists() && ! src.isDirectory()){
			  InputStream in = new FileInputStream(src);
			  OutputStream out = new FileOutputStream(dest); 
			  // Transfer bytes from in to out
			  byte[] buf = new byte[1024];
			  int len;
			  while ((len = in.read(buf)) > 0) {
				  out.write(buf, 0, len);
			  }
			  in.close();
			  out.close();
		  }
	}
	
	/**
	 * Copies a directory.
	 * 
	 * @param src The source directory.
	 * @param dest The target directory.
	 * @throws IOException
	 */
	public static void copyDir(File src, File dest) throws IOException{
		  if (src.isDirectory()){
			  if (!dest.exists())
				  dest.mkdir();

			  // copy files and subdirectories
			  String files[] = src.list();
			  for(int i = 0; i < files.length; i++)
				  copyDir(new File(src, files[i]), new File(dest, files[i]));
		  } else {
			  copyFile(src, dest);
		  }
    }
	
	/**
	 * Deletes a directory.
	 * 
	 * @param dir The directory to delete
	 * @return Returns true on success.
	 */
	public static boolean deleteDirectory(File dir) {
		if (dir.isDirectory())
			for (String child : dir.list()) 
				if (! deleteDirectory(new File(dir, child))) 
					return false;
	    
	    return dir.delete();
	}
	
	/**
	 * Unzips a zip compressed file to some output directory.
	 * @param in The InputStream.
	 * @param outputDir The output directory.
	 * @throws IOException 
	 * @throws FileNotFoundException 
	 */
	public static void unzipFile(InputStream in, File outputDir) throws FileNotFoundException, IOException {
		// name of the directory
		String directoryName = outputDir.getAbsolutePath();

		// buffer
		int BUFFER = 2048;
		// output
		BufferedOutputStream dest;
		
		// create tmp directory
		if(!outputDir.isDirectory())
			outputDir.mkdirs();
		
		// read file
		ZipInputStream zis = new ZipInputStream(new BufferedInputStream(in));
		
		// decompress file
		ZipEntry entry;
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
	
	}
	
	/**
	 * Zip compresses a directory.
	 * 
	 * @param inputDirectory The input directory.
	 * @param zipFile The zip file.
	 * @throws IOException
	 */
	public static void zipDirectory(File inputDirectory, File zipFile) throws IOException{
		FileOutputStream dest = new FileOutputStream(zipFile);
		ZipOutputStream out = new ZipOutputStream(new BufferedOutputStream(dest));
		   		
		// gather files and zip them
		constructZip(inputDirectory, out, "");
		
        out.close();
	}
	
	private static void constructZip(File file, ZipOutputStream out, String dir) throws IOException{
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
  
}
