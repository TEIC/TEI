/* 

   from http://stackoverflow.com/questions/4093130/ant-task-to-extract-image-dimensions-height-width-from-png-and-jpeg-files

 */
import org.apache.tools.ant.taskdefs.UpToDate;
import org.apache.tools.ant.types.FileSet;
import org.apache.tools.ant.types.LogLevel;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.DirectoryScanner;
import org.apache.tools.ant.Task;

import java.io.*;
import java.awt.Dimension;
import java.util.ArrayList;
import java.util.Iterator;
import javax.imageio.ImageIO;
import javax.imageio.ImageReader;
import javax.imageio.stream.ImageInputStream;
import javax.imageio.stream.FileImageInputStream;

public class ImageTask extends Task {

    private String outputFile;
    private ArrayList fileSetList = new ArrayList();
    private PrintStream outputFileStream;

    public void setOutputFile(String outputFile) {
        this.outputFile = outputFile.replace("/", File.separator);
    }

    public void addFileset(FileSet fileset) {
        fileSetList.add(fileset);
    }

    protected void validate() {
        if (outputFile == null) {
            throw new BuildException("file not set");
        }

        if (fileSetList.size() < 1) {
            throw new BuildException("fileset not set");
        }
    }

    protected void openOutputFile() throws IOException {
        FileOutputStream out = new FileOutputStream(this.outputFile);

        // Connect print stream to the output stream
        this.outputFileStream = new PrintStream(out, true, "UTF-8");

        this.outputFileStream.println("<images>");
    }

    protected void writeImgToOutputFile(String filename, Dimension dim) {
        String imgTag = "<image url=\"" + filename.replace("\\", "/")
                + "\"><height>" + dim.height + "</height><width>" + dim.width  + "</width></image>";

        this.outputFileStream.println(imgTag);
    }

    protected void closeOutputFile() {
        this.outputFileStream.println("</images>");

        this.outputFileStream.close();
    }

    @Override
    public void execute() {
        validate();

        try {
            openOutputFile();

            for (Iterator itFSets = fileSetList.iterator(); itFSets.hasNext();) {
                FileSet fs = (FileSet) itFSets.next();
                DirectoryScanner ds = fs.getDirectoryScanner(getProject());
                String[] includedFiles = ds.getIncludedFiles();
                for (int i = 0; i < includedFiles.length; i++) {
                    String filename = includedFiles[i];

                    Dimension dim = getImageDim(ds.getBasedir() + File.separator + filename);
                    if (dim != null) {
                        writeImgToOutputFile(filename, dim);
                    }
                }
            }

            closeOutputFile();
        }  catch (IOException ex) {
            log(ex.getMessage());
        }
    }

    private Dimension getImageDim(final String path) {
        Dimension result = null;
        String suffix = this.getFileSuffix(path);
        Iterator<ImageReader> iter = ImageIO.getImageReadersBySuffix(suffix);
        if (iter.hasNext()) {
            ImageReader reader = iter.next();
            try {
                ImageInputStream stream = new FileImageInputStream(new File(path));
                reader.setInput(stream);
                int width = reader.getWidth(reader.getMinIndex());
                int height = reader.getHeight(reader.getMinIndex());
                result = new Dimension(width, height);
            } catch (IOException e) {
                log(path + ": " + e.getMessage());
            } finally {
                reader.dispose();
            }
        }
        return result;
    }

    private String getFileSuffix(final String path) {
        String result = null;
        if (path != null) {
            result = "";
            if (path.lastIndexOf('.') != -1) {
                result = path.substring(path.lastIndexOf('.'));
                if (result.startsWith(".")) {
                    result = result.substring(1);
                }
            }
        }
        return result;
    }
}