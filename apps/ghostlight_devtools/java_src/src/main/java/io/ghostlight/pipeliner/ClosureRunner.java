package io.ghostlight.pipeliner;

import com.google.common.collect.Lists;
import com.google.javascript.jscomp.*;
import com.google.javascript.jscomp.Compiler;

import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.PrintWriter;
import java.util.List;

/**
 * Closure is a funny beast, and calling it from Java is not extremely well-documented.
 * There's this:
 * http://blog.bolinfest.com/2009/11/calling-closure-compiler-from-java.html
 *
 * Which teaches you how to call raw source on it, rather than multiple files. That recommends
 * later that you subclass a missing class. This, on the other hand:
 *
 * https://github.com/google/closure-compiler/wiki/FAQ#how-do-i-call-closure-compiler-from-the-java-api
 *
 * Tells you to subclass a _different_ class that is Peak Google Java monstrosity.
 *
 * I think I've been able to piece together how to simulate calling this on the command line,
 * without actually subclassing a Command Line Runner class that both requires me to simulate
 * flags AND will kill the Java process when it is done.
 *
 * Created by pablo on 5/3/15.
 */
public class ClosureRunner {

    public void minify(List<String> inputPaths, String output) {
        Compiler compiler = new Compiler(System.err);

        for (String source : inputPaths) {
            System.out.println("  Source is " + source);
        }
        System.out.println("Minifying to " + output);

        List<SourceFile> inputSources = Lists.newArrayList();
        for (String input : inputPaths) {
            SourceFile file = SourceFile.fromFile(input);
            inputSources.add(file);
        }

        CompilerOptions options = createOptions();
        String outputSrc;
        try {
            compiler.compile(CommandLineRunner.getDefaultExterns(), inputSources, options);
        } catch (IOException e) {
            System.err.println("IOException when trying to fetch Closure externs: " + e.getMessage());
            System.exit(1);
        }

        outputSrc = compiler.toSource();
        PrintWriter outWriter = null;
        try {
            outWriter = new PrintWriter(output);
            outWriter.print(outputSrc);
            outWriter.flush();
        } catch (FileNotFoundException e) {
            System.err.println("FileNotFoundError for outputWriter (js): " + e.getMessage());
            System.exit(1);
        } finally {
            if (outWriter != null) {
                outWriter.close();
            }
            System.out.println("(js) We did it, boys.");
        }
    }


    /**
     * This is more-or-less copy + pasted from the CommandLineRunner class of Closure Compiler.
     * If you actually _look_ at the number of compiler options there are you'll find a daunting
     * number, and I'm just trying to mimic the behaviour of a command-line run of it.
     *
     * It's originally a protected method, meaning I could simply subclass it, but it's expected
     * to read the flags from a main-method execution with, well, flags. I'm just setting them
     * manually here.
     */
    private CompilerOptions createOptions() {
        CompilerOptions options = new CompilerOptions();
        options.setCodingConvention(new ClosureCodingConvention());

        options.setAllowEs6Out(false);

        // Set to ADVANCED_OPTIMIZATIONS and see if it will work!
        CompilationLevel level = CompilationLevel.SIMPLE_OPTIMIZATIONS;
        level.setOptionsForCompilationLevel(options);
        level.setTypeBasedOptimizationOptions(options);

        options.setGenerateExports(false);
        options.setExportLocalPropertyDefinitions(false);

        WarningLevel wLevel = WarningLevel.DEFAULT;
        wLevel.setOptionsForWarningLevel(options);
        options.closurePass = false;

        options.jqueryPass = CompilationLevel.ADVANCED_OPTIMIZATIONS == level;
        options.setAngularPass(false);

        return options;
    }

}
