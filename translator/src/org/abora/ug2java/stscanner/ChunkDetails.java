/*
 * Udanax-Gold2Java - Translator
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003, 2005 David G Jones
 */
package org.abora.ug2java.stscanner;

public class ChunkDetails {
	public String context;
	public String contents;
	
	public String filename;
	public int lineNumber;
	public String description;

	public ChunkDetails() {
		super();
	}

	public ChunkDetails(String filename, int lineNumber, String description, String contents) {
		this(filename + ":" + lineNumber + ":" + description, contents);
		this.filename = filename;
		this.lineNumber = lineNumber;
		this.description = description;
	}

	public ChunkDetails(String context, String contents) {
		super();
		this.context = context;
		this.contents = contents;
	}
}
