package io.ghostlight.sanitizer;

import com.ericsson.otp.erlang.*;
import org.owasp.html.PolicyFactory;
import org.owasp.html.Sanitizers;

import java.util.Collection;
import java.util.List;
import java.util.concurrent.*;

import java.io.IOException;


/**
 * Helloooooo Java!
 *
 * This exists only to call the HtmlPolicyBuilder from OWASP so we can sanitize user-input.
 * ErlyDtl just may escape things we render with it, but a) better safe than sorry, and b)
 * we need to allow _some_ to get through to allow Markdown.
 *
 * Adventures in Jinterface!
 */
public class GhostlightHtmlSanitizer {

    private ExecutorService exec;
    private OtpMbox mBox;
    private PolicyFactory sanitizer;

    public GhostlightHtmlSanitizer(String nodeName, String cookie) throws IOException {
        exec = Executors.newFixedThreadPool(10);

        // Permissive -- we really just want to stop XSS.
        sanitizer = Sanitizers.FORMATTING
                        .and(Sanitizers.BLOCKS)
                        .and(Sanitizers.LINKS)
                        .and(Sanitizers.IMAGES)
                        .and(Sanitizers.STYLES);

        OtpNode node = new OtpNode(nodeName, cookie);
        mBox = node.createMbox("owasp_java_server");
    }

    public void listen() {
        System.out.println("____----~~~~////    yOu GoT sOmE jAvA iN yOuR eRlAnG...");
        while (true) {
            try {
                OtpErlangObject msg = mBox.receive();
                OtpErlangTuple tuple = (OtpErlangTuple) msg;
                OtpErlangPid from = (OtpErlangPid) tuple.elementAt(0);
                OtpErlangRef ref = (OtpErlangRef) tuple.elementAt(1);
                String dirty = ((OtpErlangString) tuple.elementAt(2)).stringValue();

                String sanitized = sanitizer.sanitize(dirty);

                OtpErlangString replyString = new OtpErlangString(sanitized);
                OtpErlangTuple outMsg = new OtpErlangTuple(new OtpErlangObject[]{mBox.self(), ref, replyString});
                mBox.send(from, outMsg);
            } catch (OtpErlangDecodeException e) {
                System.err.println("Failed to decode Erlang term. Details: " + e.getMessage());
            } catch  (OtpErlangExit e) {
                System.out.println("tHANK yOU");
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
            GhostlightHtmlSanitizer sanitizer = new GhostlightHtmlSanitizer(args[0], args[1]);
            sanitizer.listen();
        } catch (IOException e) {
            System.err.println("IOException creating the OtpNode.");
            e.printStackTrace();
            System.exit(1);
        }
    }
}
