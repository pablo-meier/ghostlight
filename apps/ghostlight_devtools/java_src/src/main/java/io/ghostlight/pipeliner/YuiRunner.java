package io.ghostlight.pipeliner;

import com.yahoo.platform.yui.compressor.CssCompressor;

import java.io.*;
import java.util.List;

/**
 * Same as ClosureRunner, but for YUI! YAY!
 *
 * Created by pablo on 5/4/15.
 */
public class YuiRunner {

    public YuiRunner() {
    }

    public void minify(List<String> inputPaths, String output) {
        for (String source : inputPaths) {
            System.out.println("  Source is " + source);
        }
        System.out.println("Minifying to " + output);


        PrintWriter outWriter = null;
        try {
            StringWriter strWriter = new StringWriter();
            for (String input : inputPaths) {
                CssCompressor compressor = new CssCompressor(new FileReader(input));
                compressor.compress(strWriter, -1);
            }
            outWriter = new PrintWriter(output);
            outWriter.print(strWriter.toString());
            outWriter.flush();
        } catch (FileNotFoundException e) {
            System.err.println("FileNotFoundError for outputWriter (css): " + e.getMessage());
            System.exit(1);
        } catch (IOException e) {
            System.err.println("IOException creating the CssCompressor (css): " + e.getMessage());
            System.exit(1);
        } finally {
            if (outWriter != null) {
                outWriter.close();
            }
            System.out.println("(css) We did it, boys");
        }
    }
}
