package org.abora.ug2java;

import java.util.ArrayList;
import java.util.List;

import junit.framework.TestCase;

import org.abora.ug2java.javatoken.JavaCallArgumentSeparator;
import org.abora.ug2java.javatoken.JavaCallEnd;
import org.abora.ug2java.javatoken.JavaCallKeywordStart;
import org.abora.ug2java.javatoken.JavaCallStart;
import org.abora.ug2java.javatoken.JavaComment;
import org.abora.ug2java.javatoken.JavaIdentifier;
import org.abora.ug2java.javatoken.IntegerLiteral;
import org.abora.ug2java.javatoken.JavaKeyword;



public class MethodBodyTest extends TestCase {

	MethodBody body;
	List tokens;
	
	protected void setUp() throws Exception {
		super.setUp();
		
		tokens = new ArrayList();
		body = new MethodBody(tokens);
	}
	
	public void testFindNumberOfCallArgsNotStartingOnACall() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaKeyword("+"));
		tokens.add(new JavaIdentifier("blah"));
		
		try {
			body.findNumberOfCallArgs(0);
		} catch (IllegalStateException e) {
			assertTrue(e.toString(), e.getMessage().indexOf("0") != -1);
		}
	}

	public void testFindNumberOfCallArgsWithNoArgs() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallStart("call"));
		tokens.add(new JavaCallEnd());
		
		assertEquals(0, body.findNumberOfCallArgs(1));
	}

	public void testFindNumberOfCallArgsWithNoArgsButAComment() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallStart("call"));
		tokens.add(new JavaComment("some kind of comment"));
		tokens.add(new JavaCallEnd());
		
		assertEquals(0, body.findNumberOfCallArgs(1));
	}

	public void testFindNumberOfCallArgsWith1Arg() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallKeywordStart("call"));
		tokens.add(new IntegerLiteral(189));
		tokens.add(new JavaCallEnd());
		
		assertEquals(1, body.findNumberOfCallArgs(1));
	}

	public void testFindNumberOfCallArgsWith2Args() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallKeywordStart("call"));
		tokens.add(new IntegerLiteral(189));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaIdentifier("blah"));
		tokens.add(new JavaCallEnd());
		
		assertEquals(2, body.findNumberOfCallArgs(1));
	}

	public void testFindNumberOfCallArgsWithEmbeddedCalls() {
		tokens.add(new JavaIdentifier("test"));
		tokens.add(new JavaCallKeywordStart("call"));
		
		// Embedded call w/2 args
		tokens.add(new JavaCallKeywordStart("innerCall"));
		tokens.add(new JavaIdentifier("blah"));
		tokens.add(new JavaCallArgumentSeparator());
		tokens.add(new JavaIdentifier("blah2"));
		tokens.add(new JavaCallEnd());
		
		tokens.add(new JavaCallEnd());
		
		assertEquals(1, body.findNumberOfCallArgs(1));
	}
}
