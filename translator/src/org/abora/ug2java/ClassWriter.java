/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */

package org.abora.ug2java;

import java.io.File;
import java.io.FileWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.StringTokenizer;

import org.abora.ug2java.javatoken.JavaToken;
import org.abora.ug2java.stscanner.ChunkDetails;



public class ClassWriter {

	private final JavaClass javaClass;
	public boolean quoteSmalltalk = true;
	public boolean shouldIndent = true;

	private int JAVADOC_MARGIN = 90;
	
	private static final boolean INCLUDE_METHOD_BODIES = true;

	public ClassWriter(JavaClass javaClass) {
		this.javaClass = javaClass;
	}
	
	protected void writeVariables(PrintWriter writer) throws Exception {
		for (Iterator iter = javaClass.getFields().iterator(); iter.hasNext();) {
			JavaField javaField = (JavaField) iter.next();
			writer.println("\tprotected " + javaField.modifiers + javaField.type + " " + javaField.name + ";");
		}
	}

	protected void writeMethodBody(MethodBody methodBody, PrintWriter writer) {
		Indentation indentation = getIndenter();
		indentation.increase();
		StringWriter stringWriter = new StringWriter();
		JavaWriter javaWriter = new JavaWriter(new PrintWriter(stringWriter), indentation);
		
			for (Iterator e = methodBody.tokens.iterator(); e.hasNext();) {
				JavaToken token = (JavaToken) e.next();
				token.write(javaWriter);
			}
			javaWriter.flush();
			writer.print(stringWriter.toString());
	}

	private Indentation getIndenter() {
		Indentation indentation;
		if (shouldIndent) {
			indentation = new SimpleIndenter();
		} else {
			indentation = new FlushIndentation();
		}
		return indentation;
	}

	private void writeMethods(PrintWriter writer) {
		for (Iterator iter = javaClass.methods.iterator(); iter.hasNext();) {
			JavaMethod javaMethod = (JavaMethod) iter.next();
			writeMethod(javaMethod, writer);
		}
	}

	public void writeMethod(JavaMethod javaMethod, PrintWriter writer) {
		if (javaMethod.name.startsWith("inspect")) {
			javaMethod.shouldInclude = false;
		}
		
		if (javaMethod.shouldInclude) {
			writeMethodJavaDoc(javaMethod, writer);
		
			writer.print("public ");
			writer.print(javaMethod.modifiers);
			writer.print(javaMethod.returnType);
			if (javaMethod.modifiers.length() > 0 || javaMethod.returnType.length() > 0) {
				writer.print(" ");
			}
			writer.println(javaMethod.name + "(" + javaMethod.params + ") {");
			
			if (INCLUDE_METHOD_BODIES) {
				writeMethodBody(javaMethod.methodBody, writer);
			} else {
				writer.write("throw new UnsupportedOperationException();");
			}
		}
		if (quoteSmalltalk) {
			writeAsQuote(writer, javaMethod.smalltalkSource.context, javaMethod.smalltalkSource.text);
		}
		if (javaMethod.shouldInclude) {
			writer.println("}");
		}
	}

	private void writeMethodJavaDoc(JavaMethod javaMethod, PrintWriter writer) {
		String comment = "";
		if (javaMethod.comment != null) {
			comment += javaMethod.comment;
		}
		if (javaMethod.isDeprecated) {
			comment += "\n@deprecated";
		}
		if (comment.length() > 0) {
			writeAsJavadocComment(writer, comment);			
		}
	}

	private void writeImports(PrintWriter writer) {
		for (Iterator iterator = javaClass.importedPackages.iterator(); iterator.hasNext();) {
			String importPackage = (String) iterator.next();
			if (!importPackage.equals(javaClass.getPackage())) {
				writer.println("import " + importPackage + ";");
			}
		}
	}

	protected void writeFileComment(PrintWriter writer) {
		final String fileComment =
			"Abora-Gold\n"
				+ "Part of the Abora hypertext project: http://www.abora.org\n"
				+ "Copyright 2003, 2005 David G Jones\n"
				+ " \n"
				+ "Translated from Udanax-Gold source code: http://www.udanax.com\n"
				+ "Copyright 1979-1999 Udanax.com. All rights reserved";
		writeAsComment(writer, fileComment);
	}

	public String writeClassDefinition() throws Exception {
		StringWriter stringWriter = new StringWriter();
		PrintWriter writer = new PrintWriter(stringWriter);
		try {
			writer.println();
			if (javaClass.comment != null) {
				writeAsJavadocComment(writer, javaClass.comment);
			}
			writer.println("public class " + javaClass.className + " extends " + javaClass.superclassName + " {");
	
			writeVariables(writer);
	
			for (Enumeration e = javaClass.classQuotes.elements(); e.hasMoreElements();) {
				ChunkDetails comment = (ChunkDetails) e.nextElement();
				writeAsQuote(writer, comment.context, comment.contents);
			}
			writeMethods(writer);
			writer.println("}");
		} finally {
			writer.close();
		}
		return stringWriter.toString();
	}

	protected void writeAsQuote(PrintWriter writer, String context, String comment) {
		comment = stringReplaceWith(comment, "/*", "/-");
		comment = stringReplaceWith(comment, "*/", "-/");
	
		writer.println("/*");
		writer.println(context);
		for (StringTokenizer tokenizer = new StringTokenizer(comment, "\n"); tokenizer.hasMoreTokens();) {
			writer.println(tokenizer.nextToken());
		}
		writer.println("*/");
	}

	protected void writeAsJavadocComment(PrintWriter writer, String comment) {
		writer.println("/**");
		for (StringTokenizer tokenizer = new StringTokenizer(comment, "\n"); tokenizer.hasMoreTokens();) {
			String line = tokenizer.nextToken().trim();
			int start = 0;
			while (start < line.length()) {
				int end = line.length() - 1;
				if (start + JAVADOC_MARGIN < line.length()) {
					end = Math.min(start + JAVADOC_MARGIN, end);
					while (end > start && !Character.isWhitespace(line.charAt(end))) {
						end -= 1;
					}
					while (end > start && Character.isWhitespace(line.charAt(end))) {
						end -= 1;
					}
					if (end == start) {
						end = line.length() - 1;
					}
				}
				writer.println(" * " + line.substring(start, end + 1));
				start = end + 1;
				while (start < line.length() - 1 && Character.isWhitespace(line.charAt(start))) {
					start += 1;
				}
			}
		}
		writer.println(" */");
	}

	protected void writeAsComment(PrintWriter writer, String comment) {
		writer.println("/*");
		for (StringTokenizer tokenizer = new StringTokenizer(comment, "\n"); tokenizer.hasMoreTokens();) {
			String line = tokenizer.nextToken().trim();
			writer.println(" * " + line);
		}
		writer.println(" */");
	}

	public void write(String baseDirectory) throws Exception {
		System.out.println("Writing class: " + javaClass.getPackage() + "." + javaClass.className);
	
		File dir = new File(baseDirectory, javaClass.getPackageDirectory());
		dir.mkdirs();
	
		File javaFile = new File(dir, javaClass.className + ".java");
		FileWriter fileWriter = new FileWriter(javaFile);
		PrintWriter writer = new PrintWriter(fileWriter);
		try {
			String classDefinition = writeClassDefinition();
	
			writeFileComment(writer);
			writer.println("package " + javaClass.getPackage() + ";");
			writer.println();
			writeImports(writer);
			writer.println();
			writer.print(classDefinition);
		} finally {
			fileWriter.close();
		}
	}

	protected String stringReplaceWith(String s, String find, String replaceWith) {
		//FIXME use String.replaceAll() instead
		StringBuffer buffer = new StringBuffer();
		int start = 0;
		int match;
		while ((match = s.indexOf(find, start)) != -1) {
			buffer.append(s.substring(start, match));
			buffer.append(replaceWith);
			start = match + find.length();
		}
		buffer.append(s.substring(start));
		return buffer.toString();
	}

}
