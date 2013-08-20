;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: feature-based conditional expansion
;;;Date: Tue Aug 20, 2013
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2013 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY or  FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received a  copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!r6rs
(library (vicare xml expat cond-expand)
  (export vicare-expat-features)
  (import (only (vicare language-extensions cond-expand helpers)
		define-cond-expand-identifiers-helper)
    (vicare xml expat features)
    (for (vicare xml expat)
	 (meta -1)))


(define-cond-expand-identifiers-helper vicare-expat-features
  (XML_ParserCreate				HAVE_XML_PARSERCREATE)
  (XML_ParserCreateNS				HAVE_XML_PARSERCREATENS)
  #;(XML_ParserCreate_MM			HAVE_XML_ParserCreate_MM)
  (XML_ParserReset				HAVE_XML_PARSERRESET)
  (XML_DefaultCurrent				HAVE_XML_DEFAULTCURRENT)
  (XML_SetReturnNSTriplet			HAVE_XML_SETRETURNNSTRIPLET)
  (XML_SetUserData				HAVE_XML_SETUSERDATA)
  (XML_GetUserData				HAVE_XML_GETUSERDATA)
  (XML_SetEncoding				HAVE_XML_SETENCODING)
  (XML_UseForeignDTD				HAVE_XML_USEFOREIGNDTD)
  (XML_SetBase					HAVE_XML_SETBASE)
  (XML_GetBase					HAVE_XML_GETBASE)
  (XML_GetSpecifiedAttributeCount		HAVE_XML_GETSPECIFIEDATTRIBUTECOUNT)
  (XML_GetIdAttributeIndex			HAVE_XML_GETIDATTRIBUTEINDEX)
  (XML_Parse					HAVE_XML_PARSE)
  (XML_GetBuffer				HAVE_XML_GETBUFFER)
  (XML_ParseBuffer				HAVE_XML_PARSEBUFFER)
  (XML_StopParser				HAVE_XML_STOPPARSER)
  (XML_ResumeParser				HAVE_XML_RESUMEPARSER)
  (XML_GetParsingStatus				HAVE_XML_GETPARSINGSTATUS)
  (XML_ExternalEntityParserCreate		HAVE_XML_EXTERNALENTITYPARSERCREATE)
  (XML_SetParamEntityParsing			HAVE_XML_SETPARAMENTITYPARSING)
  (XML_GetErrorCode				HAVE_XML_GETERRORCODE)
  (XML_GetCurrentLineNumber			HAVE_XML_GETCURRENTLINENUMBER)
  (XML_GetCurrentColumnNumber			HAVE_XML_GETCURRENTCOLUMNNUMBER)
  (XML_GetCurrentByteIndex			HAVE_XML_GETCURRENTBYTEINDEX)
  (XML_GetCurrentByteCount			HAVE_XML_GETCURRENTBYTECOUNT)
  (XML_GetInputContext				HAVE_XML_GETINPUTCONTEXT)
  (XML_FreeContentModel				HAVE_XML_FREECONTENTMODEL)
  (XML_ParserFree				HAVE_XML_PARSERFREE)
  (XML_ErrorString				HAVE_XML_ERRORSTRING)
  (XML_ExpatVersion				HAVE_XML_EXPATVERSION)
  (XML_ExpatVersionInfo				HAVE_XML_EXPATVERSIONINFO)
  (XML_GetFeatureList				HAVE_XML_GETFEATURELIST)

  (XML_SetAttlistDeclHandler			HAVE_XML_SETATTLISTDECLHANDLER)
  (XML_SetCdataSectionHandler			HAVE_XML_SETCDATASECTIONHANDLER)
  (XML_SetCharacterDataHandler			HAVE_XML_SETCHARACTERDATAHANDLER)
  (XML_SetCommentHandler			HAVE_XML_SETCOMMENTHANDLER)
  (XML_SetDefaultHandler			HAVE_XML_SETDEFAULTHANDLER)
  (XML_SetDefaultHandlerExpand			HAVE_XML_SETDEFAULTHANDLEREXPAND)
  (XML_SetDoctypeDeclHandler			HAVE_XML_SETDOCTYPEDECLHANDLER)
  (XML_SetElementDeclHandler			HAVE_XML_SETELEMENTDECLHANDLER)
  (XML_SetElementHandler			HAVE_XML_SETELEMENTHANDLER)
  (XML_SetEndCdataSectionHandler		HAVE_XML_SETENDCDATASECTIONHANDLER)
  (XML_SetEndDoctypeDeclHandler			HAVE_XML_SETENDDOCTYPEDECLHANDLER)
  (XML_SetEndElementHandler			HAVE_XML_SETENDELEMENTHANDLER)
  (XML_SetEndNamespaceDeclHandler		HAVE_XML_SETENDNAMESPACEDECLHANDLER)
  (XML_SetEntityDeclHandler			HAVE_XML_SETENTITYDECLHANDLER)
  (XML_SetExternalEntityRefHandler		HAVE_XML_SETEXTERNALENTITYREFHANDLER)
  (XML_SetNamespaceDeclHandler			HAVE_XML_SETNAMESPACEDECLHANDLER)
  (XML_SetNotStandaloneHandler			HAVE_XML_SETNOTSTANDALONEHANDLER)
  (XML_SetNotationDeclHandler			HAVE_XML_SETNOTATIONDECLHANDLER)
  (XML_SetProcessingInstructionHandler		HAVE_XML_SETPROCESSINGINSTRUCTIONHANDLER)
  (XML_SetSkippedEntityHandler			HAVE_XML_SETSKIPPEDENTITYHANDLER)
  (XML_SetStartCdataSectionHandler		HAVE_XML_SETSTARTCDATASECTIONHANDLER)
  (XML_SetStartDoctypeDeclHandler		HAVE_XML_SETSTARTDOCTYPEDECLHANDLER)
  (XML_SetStartElementHandler			HAVE_XML_SETSTARTELEMENTHANDLER)
  (XML_SetStartNamespaceDeclHandler		HAVE_XML_SETSTARTNAMESPACEDECLHANDLER)
;;;interface not implemented
  #;(XML_UnknownEncodingHandler			HAVE_XML_UNKNOWNENCODINGHANDLER)
  (XML_SetUnparsedEntityDeclHandler		HAVE_XML_SETUNPARSEDENTITYDECLHANDLER)
  (XML_SetXmlDeclHandler			HAVE_XML_SETXMLDECLHANDLER)

  ;; auxiliary callback functions
  (XML_SetExternalEntityRefHandlerArg		HAVE_XML_SETEXTERNALENTITYREFHANDLERARG)
  (XML_UseParserAsHandlerArg			HAVE_XML_USEPARSERASHANDLERARG)

  ;;Callback makers.  These are  available if the corresponding function
  ;;to register the callback is available.
  (XML_AttlistDeclHandler			HAVE_XML_SETATTLISTDECLHANDLER)
  (XML_CharacterDataHandler			HAVE_XML_SETCHARACTERDATAHANDLER)
  (XML_CommentHandler				HAVE_XML_SETCOMMENTHANDLER)
  (XML_DefaultHandler				HAVE_XML_SETDEFAULTHANDLER)
  (XML_ElementDeclHandler			HAVE_XML_SETELEMENTDECLHANDLER)
  (XML_EndCdataSectionHandler			HAVE_XML_SETENDCDATASECTIONHANDLER)
  (XML_EndDoctypeDeclHandler			HAVE_XML_SETENDDOCTYPEDECLHANDLER)
  (XML_EndElementHandler			HAVE_XML_SETENDELEMENTHANDLER)
  (XML_EndNamespaceDeclHandler			HAVE_XML_SETENDNAMESPACEDECLHANDLER)
  (XML_EntityDeclHandler			HAVE_XML_SETENTITYDECLHANDLER)
  (XML_ExternalEntityRefHandler			HAVE_XML_SETEXTERNALENTITYREFHANDLER)
  (XML_NotStandaloneHandler			HAVE_XML_SETNOTSTANDALONEHANDLER)
  (XML_NotationDeclHandler			HAVE_XML_SETNOTATIONDECLHANDLER)
  (XML_ProcessingInstructionHandler		HAVE_XML_SETPROCESSINGINSTRUCTIONHANDLER)
  (XML_SkippedEntityHandler			HAVE_XML_SETSKIPPEDENTITYHANDLER)
  (XML_StartCdataSectionHandler			HAVE_XML_SETSTARTCDATASECTIONHANDLER)
  (XML_StartDoctypeDeclHandler			HAVE_XML_SETSTARTDOCTYPEDECLHANDLER)
  (XML_StartElementHandler			HAVE_XML_SETSTARTELEMENTHANDLER)
  (XML_StartNamespaceDeclHandler		HAVE_XML_SETSTARTNAMESPACEDECLHANDLER)
;;;interface not implemented
  #;(XML_UnknownEncodingHandler			HAVE_XML_SETUNKNOWNENCODINGHANDLER)
  (XML_UnparsedEntityDeclHandler		HAVE_XML_SETUNPARSEDENTITYDECLHANDLER)
  (XML_XmlDeclHandler				HAVE_XML_SETXMLDECLHANDLER))


;;;; done

)

;;; end of file
