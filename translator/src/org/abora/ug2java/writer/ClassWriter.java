/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */

package org.abora.ug2java.writer;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.StringTokenizer;

import org.abora.ug2java.JavaClass;
import org.abora.ug2java.JavaField;
import org.abora.ug2java.JavaMethod;
import org.abora.ug2java.MethodBody;
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

	private void writeStaticBlocks(PrintWriter writer) {
		for (Iterator iter = javaClass.staticBlocks.iterator(); iter.hasNext();) {
			JavaMethod javaMethod = (JavaMethod) iter.next();
			writeStaticBlock(javaMethod, writer);
		}
	}
	
	private void writeMethods(PrintWriter writer) {
		for (Iterator iter = javaClass.methods.iterator(); iter.hasNext();) {
			JavaMethod javaMethod = (JavaMethod) iter.next();
			writeMethod(javaMethod, writer);
		}
	}

	
	public void writeStaticBlock(JavaMethod javaMethod, PrintWriter writer) {
		writer.println("static {");
		writeMethodBody(javaMethod.methodBody, writer);
		if (quoteSmalltalk) {
			writeAsQuote(writer, javaMethod.smalltalkSource.context, javaMethod.smalltalkSource.text);
		}
		writer.println("}");
	}

		public void writeMethod(JavaMethod javaMethod, PrintWriter writer) {
		if (javaMethod.shouldInclude) {
			writeMethodJavaDoc(javaMethod, writer);
		
			writeMethodSignature(javaMethod, writer);
			
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

	private void writeMethodSignature(JavaMethod javaMethod, PrintWriter writer) {
		writer.print("public ");
		writer.print(javaMethod.modifiers);
		writer.print(javaMethod.returnType);
		if (javaMethod.modifiers.length() > 0 || javaMethod.returnType.length() > 0) {
			writer.print(" ");
		}
		writer.print(javaMethod.name + "(");
		for (Iterator iter = javaMethod.parameters.iterator(); iter.hasNext();) {
			JavaField element = (JavaField) iter.next();
			if (element.modifiers != null && element.modifiers.length() > 0) {
				writer.print(element.modifiers);
				writer.print(" ");
			}
			writer.print(element.type);
			writer.print(" ");
			writer.print(element.name);
			if (iter.hasNext()) {
				writer.print(", ");
			}
		}
		writer.println(") {");
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
			writeStaticBlocks(writer);
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
	
		File dir = new File(baseDirectory, javaClass.getPackageDirectory());
		dir.mkdirs();
	
		File javaFile = new File(dir, javaClass.className + ".java");
		String generatedContents = generate();
		String existingContents = readExistingFile(javaFile);
		if (!generatedContents.equals(existingContents)) {
			System.out.println("Writing class: " + javaClass.getPackage() + "." + javaClass.className+" ["+summarizeDifference(existingContents, generatedContents)+"]");
//			javaFile.delete();
			writeContents(generatedContents, javaFile);
		}
	}
	
	private String summarizeDifference(String existingContents, String generatedContents) {
		if (existingContents == null) {
			return "New";
		} else {
			for (int i = 0; i < Math.min(existingContents.length(), generatedContents.length()); i++) {
				if (existingContents.charAt(i) != generatedContents.charAt(i)) {
					String e = highlightText(existingContents, i);
					String g = highlightText(generatedContents, i);
					return "'"+e+"' -> '"+g+"' "+i;
				}
			}
			if (existingContents.length() != generatedContents.length()) {
				return "Differente Size";
			}
		}
		return "";
	}
	
	private String highlightText(String text, int pointOfHighlight) {
		String highlight = text.substring(pointOfHighlight - 10, pointOfHighlight + 10);
		highlight = highlight.replace('\n', '.');
		highlight = highlight.replace('\r', '.');
		return highlight;
	}

	private void writeContents(String contents, File javaFile) throws IOException {
		OutputStreamWriter writer = new OutputStreamWriter(new FileOutputStream(javaFile, false), "UTF-8");
		try {
			writer.write(contents);
		} finally {
			writer.close();
		}
	}

	private String readExistingFile(File javaFile) throws IOException {
		if (!javaFile.exists()) {
			return null;
		}
		InputStreamReader reader = new InputStreamReader(new FileInputStream(javaFile), "UTF-8");
		try {
			StringBuffer stringBuffer = new StringBuffer();
			char[] buffer = new char[2048];
			int read;
			
			while ((read = reader.read(buffer)) != -1) {
				stringBuffer.append(buffer, 0, read);
			}
			return stringBuffer.toString();
		} finally {
			reader.close();
		}
	}

	private String generate() throws Exception {
		StringWriter stringWriter = new StringWriter();
		PrintWriter printWriter = new PrintWriter(stringWriter);
		try {
			write(printWriter);
			printWriter.flush();
			return stringWriter.toString();
		} finally {
			printWriter.close();
		}
	}

	public void write(PrintWriter writer) throws Exception {
		String classDefinition = writeClassDefinition();

		writeFileComment(writer);
		writer.println("package " + javaClass.getPackage() + ";");
		writer.println();
		writeImports(writer);
		writer.println();
		writer.print(classDefinition);
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
