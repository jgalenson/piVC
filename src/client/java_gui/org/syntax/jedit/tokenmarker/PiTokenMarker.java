/*
 * JavaTokenMarker.java - Java token marker
 * Copyright (C) 1999 Slava Pestov
 *
 * You may use and modify this package for any purpose. Redistribution is
 * permitted, in both source and binary form, provided that this notice
 * remains intact in all source distributions of this package.
 */

package org.syntax.jedit.tokenmarker;

import org.syntax.jedit.*;
import javax.swing.text.Segment;

/**
 * Java token marker.
 *
 * @author Slava Pestov
 * @version $Id: JavaTokenMarker.java,v 1.5 1999/12/13 03:40:30 sp Exp $
 */
public class PiTokenMarker extends CTokenMarker
{
	public PiTokenMarker()
	{
		super(false,getKeywords());
	}

	public static KeywordMap getKeywords()
	{
		if(piKeywords == null)
		{
			piKeywords = new KeywordMap(false);
			piKeywords.add("int",Token.KEYWORD3);
			piKeywords.add("float",Token.KEYWORD3);
			piKeywords.add("boolean",Token.KEYWORD3);
			piKeywords.add("void",Token.KEYWORD3);
			piKeywords.add("class",Token.KEYWORD3);
			piKeywords.add("break",Token.KEYWORD1);
			piKeywords.add("continue",Token.KEYWORD1);
			piKeywords.add("else",Token.KEYWORD1);
			piKeywords.add("for",Token.KEYWORD1);
			piKeywords.add("if",Token.KEYWORD1);
			piKeywords.add("new",Token.KEYWORD1);
			piKeywords.add("return",Token.KEYWORD1);
			piKeywords.add("while",Token.KEYWORD1);
			piKeywords.add("true",Token.LITERAL2);
			piKeywords.add("false",Token.LITERAL2);
			piKeywords.add("@",Token.LABEL);
			piKeywords.add("#",Token.LABEL);
			piKeywords.add("pre",Token.LABEL);
			piKeywords.add("post",Token.LABEL);
			piKeywords.add("forall",Token.KEYWORD1);
			piKeywords.add("exists",Token.KEYWORD1);
			piKeywords.add("sorted",Token.KEYWORD1);
			piKeywords.add("partitioned",Token.KEYWORD1);
			
		}
		return piKeywords;
	}

	// private members
	private static KeywordMap piKeywords;
}
