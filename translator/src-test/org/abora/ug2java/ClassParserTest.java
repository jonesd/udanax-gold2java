package org.abora.ug2java;

import org.abora.ug2java.stscanner.ChunkDetails;


public class ClassParserTest extends WriteMethodTestCase {

	public ClassParserTest(String name) {
		super(name);
	}

	public void testParseGetOrMakeCxxClassDescriptionChunk() {
		
		ChunkDetails chunk = new ChunkDetails("context", "(Test getOrMakeCxxClassDescription)\n" + 
				"	attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!\n" + 
				"");
		javaClass.classQuotes.add(chunk);
		
		ClassParser parser = new ClassParser();
		parser.setJavaClass(javaClass);
		
		parser.parseGetOrMakeCxxClassDescriptionChunk(chunk);
		
		JavaMethod method = javaClass.getMethod("initializeClassAttributes");
		assertNotNull("generated initializeClassAttributes", method);
		parser.transformMethod(method);

		String methodText = writeMethod(method);
		assertBodyEquals("public static void initializeClassAttributes() {\nAboraSupport.findAboraClass(Test.class).setAttributes( new Set().add(\"CONCRETE\").add(\"NOTATYPE\"));\n}\n", methodText);
	}
	
	public void testTransformSmalltalkSymbolToJava() {
		assertEquals("SIMPLE", ClassParser.transformSmalltalkSymbolToJava("simple"));
		assertEquals("MORE_COMPLICATED", ClassParser.transformSmalltalkSymbolToJava("moreComplicated"));
		//TODO test :
		
	}
}
