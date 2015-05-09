package io.ghostlight.pipeliner;

import com.ericsson.otp.erlang.*;
import com.google.common.collect.Lists;

import java.io.IOException;
import java.util.ArrayList;

/**
 * Little Jinterface class that lets us run the Google Closure compiler and YUI
 * Compressor on JS and CSS, respectively.
 *
 * Simple wrappers to the CLI of those tools would be a bit of a pain because
 * a) they'd take a while to to start/warm-up the JVM for each individual run,
 * and b) the ports interface requires us to have a program that doesn't terminate,
 * meaning I'm either writing something in a language I don't care for (a loop in
 * bash that takes values in via stdin?), one that is Yet Another Dependency
 * (Ruby or Python?), or just calling Linux command-line invocations from Erlang
 * directly, which feels real dirty.
 *
 * So I'll just do the slightly more laborious, but More Pure, decision of keeping
 * a running instance of the JVM that listens, acts like an Erlang remote node
 * (we're already doing this for the Markdown…) and doesn't require generating/mungling
 * UNIX commands.
 *
 * Created by pablo on 5/3/15.
 */
public class GhostlightPipeliner {

    private OtpMbox mBox;
    private YuiRunner yuiRunner;
    private ClosureRunner closureRunner;

    GhostlightPipeliner(String nodeName, String cookie) throws IOException {
        OtpNode node = new OtpNode(nodeName, cookie);
        mBox = node.createMbox("devtools_java_server");
        closureRunner = new ClosureRunner();
        yuiRunner = new YuiRunner();
    }

    /**
     * We listen for the following messages:
     *
     * {From, Ref, js, [InputFiles], OutFile}
     * {From, Ref, css, [InputFiles], OutFile}
     */
    public void listen() {
        System.out.println("oppa gangnam style");
        while (true) {
            try {
                OtpErlangObject msg = mBox.receive();
                OtpErlangTuple tuple = (OtpErlangTuple) msg;

                OtpErlangPid from = (OtpErlangPid) tuple.elementAt(0);
                OtpErlangRef ref = (OtpErlangRef) tuple.elementAt(1);
                OtpErlangAtom type = (OtpErlangAtom) tuple.elementAt(2);
                OtpErlangList inputs = (OtpErlangList) tuple.elementAt(3);
                OtpErlangString output = (OtpErlangString) tuple.elementAt(4);

                ArrayList<String> strInputs = Lists.newArrayList();
                for (OtpErlangObject obj : inputs.elements()) {
                    strInputs.add(((OtpErlangString) obj).stringValue());
                }


                String returnMessage = "ok";
                if (type.atomValue().equals("js")) {
                    closureRunner.minify(strInputs, output.stringValue());
                } else if (type.atomValue().equals("css")) {
                    yuiRunner.minify(strInputs, output.stringValue());
                } else {
                    returnMessage = "wat? Requested filetype " + type.atomValue();
                }

                OtpErlangAtom ok = new OtpErlangAtom(returnMessage);
                OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{mBox.self(), ref, ok});
                mBox.send(from, outMsg);
            } catch (OtpErlangDecodeException e) {
                System.err.println("Failed to decode Erlang term. Details…" + e.getMessage());
            } catch (OtpErlangExit e) {
                System.out.println("E.T. think he so slick.");
                System.exit(0);
            }
        }
    }

    public static void main(String[] args) {

        if (args.length != 2) {
            System.err.println("Wrong number of arguments:");
            System.err.println("Expected: nodeName cookie");
            System.exit(1);
        }

        try {
            GhostlightPipeliner pipeliner = new GhostlightPipeliner(args[0], args[1]);
            pipeliner.listen();
            System.out.println("Hello World!");
        } catch (IOException e) {
            System.err.println("IOException creating GhostlightPipeline");
            e.printStackTrace();
            System.exit(1);
        }
    }
}
