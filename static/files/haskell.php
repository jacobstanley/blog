<?php
/*************************************************************************************
 * haskell.php
 * ----------
 * Author: Jason Dagit (dagit@codersbase.com) based on ocaml.php by Flaie (fireflaie@gmail.com)
 *         Jacob Stanley (http://jystic.com)
 * Copyright: (c) 2005 Flaie, Nigel McNie (http://qbnz.com/highlighter)
 * Release Version: 1.0.8.3
 * Date Started: 2005/08/27
 *
 * Haskell language file for GeSHi.
 *
 * CHANGES
 * -------
 * 2005/08/27 (1.0.0)
 *   -  First Release
 * 2010/05/21 (1.1.0)
 *   -  Added github style highlighting
 * 2010/05/23 (1.1.1)
 *   - Fixed highlighting of brackets inside quotes
 *************************************************************************************
 *
 *   This file is part of GeSHi.
 *
 *   GeSHi is free software; you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation; either version 2 of the License, or
 *   (at your option) any later version.
 *
 *   GeSHi is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with GeSHi; if not, write to the Free Software
 *   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 ************************************************************************************/

$language_data = array (
    'LANG_NAME' => 'Haskell',
    'COMMENT_SINGLE' => array( 1 => '--'),
    'COMMENT_MULTI' => array('{-' => '-}'),
    'COMMENT_REGEXP' => array(2 => "/-->/"),
    'CASE_KEYWORDS' => 0,
    'QUOTEMARKS' => array('"'),
    'ESCAPE_CHAR' => "\\",
    'KEYWORDS' => array(
        /* main haskell keywords */
        1 => array(
            'as',
            'case', 'of', 'class', 'data', 'default',
            'deriving', 'do', 'forall', 'hiding', 'if', 'then',
            'else', 'infix', 'infixl', 'infixr',
            'instance', 'let', 'in', 'newtype',
            'type', 'where'
            ),
        ),
    /* highlighting symbols is really important in Haskell */
    'SYMBOLS' => array(
        '`', '~', '!', '@', '#', '$', '%', '^', '&',
        '*', '-', '+', '=', '?', '>', '<', '.',
        ':', '/', '\\', '|'
        ),
    'CASE_SENSITIVE' => array(
        GESHI_COMMENTS => false,
        1 => true,
        ),
    'STYLES' => array(
        'KEYWORDS' => array(
            1 => 'color: #000; font-weight: bold;',
            ),
        'COMMENTS' => array(
            1 => 'color: #998; font-style: italic;',
            2 => 'color: #998; font-style: italic;',
            'MULTI' => 'color: #998; font-style: italic;'
            ),
        'ESCAPE_CHAR' => array(
            0 => 'background-color: #3cb371; font-weight: bold;'
            ),
        'BRACKETS' => array(
            ),
        'STRINGS' => array(
            0 => 'color: #d14;'
            ),
        'NUMBERS' => array(
            0 => 'color: #099;'
            ),
        'METHODS' => array(
            ),
        'REGEXPS' => array(
            0 => 'color: #555;',
            1 => 'color: #000; font-weight: bold;',
            2 => 'color: #458; font-weight: bold;',
            3 => 'color: #900; font-weight: bold;',
            4 => 'color: #900; font-weight: bold;',
            5 => 'color: #900; font-weight: bold;',
            6 => 'color: #d14;',
            7 => 'color: #283352;',
            ),
        'SYMBOLS' => array(
            0 => 'color: #000; font-weight: bold;',
            ),
        'SCRIPT' => array(
            )
        ),
    'OOLANG' => false,
    'OBJECT_SPLITTERS' => array(
        ),
    'REGEXPS' => array(
        // imports
        0 => array(
            GESHI_SEARCH    => '((module\s+)|(import\s+)(qualified\s+)?)(.+)$',
            GESHI_REPLACE   => '\\5',
            GESHI_MODIFIERS => 'm',
            GESHI_BEFORE    => '\\1',
            GESHI_AFTER     => '',
            ),
        // import/qualified keywords
        1 => array(
            GESHI_SEARCH    => '(module|import|qualified)',
            GESHI_REPLACE   => '\\1',
            GESHI_MODIFIERS => '',
            GESHI_BEFORE    => '',
            GESHI_AFTER     => '',
            ),
        // types, need to ignore geshi escapes <PIPE>, <SEMI>, REG3XP1!>
        2 => array(
            GESHI_SEARCH    => '(<[A-Z]+>|<\|!REG3XP\d*!>.*?\|>)|(\b[A-Z][a-zA-Z0-9_\']*)',
            GESHI_REPLACE   => '\\2',
            GESHI_MODIFIERS => '',
            GESHI_BEFORE    => '\\1',
            GESHI_AFTER     => '',
            ),
        // function defs
        3 => array(
            GESHI_SEARCH    => '^([a-z][a-zA-Z0-9_\']*)',
            GESHI_REPLACE   => '\\1',
            GESHI_MODIFIERS => 'm',
            GESHI_BEFORE    => '',
            GESHI_AFTER     => '',
            ),
        // function defs at the start of the file (workaround)
        4 => array(
            GESHI_SEARCH    => '^(\s)([a-z][a-zA-Z0-9_\']*)',
            GESHI_REPLACE   => '\\2',
            GESHI_MODIFIERS => '',
            GESHI_BEFORE    => '\\1',
            GESHI_AFTER     => '',
            ),
        // lambda's
        5 => array(
            GESHI_SEARCH    => '(\\\\)([a-z])',
            GESHI_REPLACE   => '\\1',
            GESHI_MODIFIERS => '',
            GESHI_BEFORE    => '',
            GESHI_AFTER     => '\\2',
            ),
        // quoted characters
        6 => array(
            GESHI_SEARCH    => '(\'..?\')',
            GESHI_REPLACE   => '\\1',
            GESHI_MODIFIERS => '',
            GESHI_BEFORE    => '',
            GESHI_AFTER     => '',
            ),
        // brackets/unit
        7 => array(
            GESHI_SEARCH    => '([^\\\'])([\(\)\[\],]+)',
            GESHI_REPLACE   => '\\2',
            GESHI_MODIFIERS => '',
            GESHI_BEFORE    => '\\1',
            GESHI_AFTER     => '',
            ),
        ),
    'STRICT_MODE_APPLIES' => GESHI_NEVER,
    'SCRIPT_DELIMITERS' => array(
        ),
    'HIGHLIGHT_STRICT_BLOCK' => array(
        )
);

?>
