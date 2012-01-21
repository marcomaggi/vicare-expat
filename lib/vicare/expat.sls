;;; -*- coding: utf-8-unix -*-
;;;
;;;Part of: Vicare/Expat
;;;Contents: Expat binding backend
;;;Date: Sat Jan 21, 2012
;;;
;;;Abstract
;;;
;;;
;;;
;;;Copyright (C) 2012 Marco Maggi <marco.maggi-ipsu@poste.it>
;;;
;;;This program is free software:  you can redistribute it and/or modify
;;;it under the terms of the  GNU General Public License as published by
;;;the Free Software Foundation, either version 3 of the License, or (at
;;;your option) any later version.
;;;
;;;This program is  distributed in the hope that it  will be useful, but
;;;WITHOUT  ANY   WARRANTY;  without   even  the  implied   warranty  of
;;;MERCHANTABILITY  or FITNESS FOR  A PARTICULAR  PURPOSE.  See  the GNU
;;;General Public License for more details.
;;;
;;;You should  have received  a copy of  the GNU General  Public License
;;;along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;


#!vicare
#!(load-shared-library "vicare-expat")
(library (vicare expat)
  (export
    XML_SetElementDeclHandler
    XML_SetAttlistDeclHandler
    XML_SetXmlDeclHandler
    XML_ParserCreate
    XML_ParserCreateNS
    XML_ParserCreate_MM
    XML_ParserReset
    XML_SetEntityDeclHandler
    XML_SetElementHandler
    XML_SetStartElementHandler
    XML_SetEndElementHandler
    XML_SetCharacterDataHandler
    XML_SetProcessingInstructionHandler
    XML_SetCommentHandler
    XML_SetCdataSectionHandler
    XML_SetStartCdataSectionHandler
    XML_SetEndCdataSectionHandler
    XML_SetDefaultHandler
    XML_SetDefaultHandlerExpand
    XML_SetDoctypeDeclHandler
    XML_SetStartDoctypeDeclHandler
    XML_SetEndDoctypeDeclHandler
    XML_SetUnparsedEntityDeclHandler
    XML_SetNotationDeclHandler
    XML_SetNamespaceDeclHandler
    XML_SetStartNamespaceDeclHandler
    XML_SetEndNamespaceDeclHandler
    XML_SetNotStandaloneHandler
    XML_SetExternalEntityRefHandler
    XML_SetExternalEntityRefHandlerArg
    XML_SetSkippedEntityHandler
    XML_SetUnknownEncodingHandler
    XML_DefaultCurrent
    XML_SetReturnNSTriplet
    XML_SetUserData
    XML_SetEncoding
    XML_UseParserAsHandlerArg
    XML_UseForeignDTD
    XML_SetBase
    XML_GetBase
    XML_GetSpecifiedAttributeCount
    XML_GetIdAttributeIndex
    XML_Parse
    XML_GetBuffer
    XML_ParseBuffer
    XML_StopParser
    XML_ResumeParser
    XML_GetParsingStatus
    XML_ExternalEntityParserCreate
    XML_SetParamEntityParsing
    XML_GetErrorCode
    XML_GetCurrentLineNumber
    XML_GetCurrentColumnNumber
    XML_GetCurrentByteIndex
    XML_GetCurrentByteCount
    XML_GetInputContext
    XML_FreeContentModel
    XML_MemMalloc
    XML_MemRealloc
    XML_MemFree
    XML_ParserFree
    XML_ErrorString
    XML_ExpatVersion
    XML_GetFeatureList)
  (import (vicare))


;;;; code


    XML_SetElementDeclHandler
    XML_SetAttlistDeclHandler
    XML_SetXmlDeclHandler
    XML_ParserCreate
    XML_ParserCreateNS
    XML_ParserCreate_MM
    XML_ParserReset
    XML_SetEntityDeclHandler
    XML_SetElementHandler
    XML_SetStartElementHandler
    XML_SetEndElementHandler
    XML_SetCharacterDataHandler
    XML_SetProcessingInstructionHandler
    XML_SetCommentHandler
    XML_SetCdataSectionHandler
    XML_SetStartCdataSectionHandler
    XML_SetEndCdataSectionHandler
    XML_SetDefaultHandler
    XML_SetDefaultHandlerExpand
    XML_SetDoctypeDeclHandler
    XML_SetStartDoctypeDeclHandler
    XML_SetEndDoctypeDeclHandler
    XML_SetUnparsedEntityDeclHandler
    XML_SetNotationDeclHandler
    XML_SetNamespaceDeclHandler
    XML_SetStartNamespaceDeclHandler
    XML_SetEndNamespaceDeclHandler
    XML_SetNotStandaloneHandler
    XML_SetExternalEntityRefHandler
    XML_SetExternalEntityRefHandlerArg
    XML_SetSkippedEntityHandler
    XML_SetUnknownEncodingHandler
    XML_DefaultCurrent
    XML_SetReturnNSTriplet
    XML_SetUserData
    XML_SetEncoding
    XML_UseParserAsHandlerArg
    XML_UseForeignDTD
    XML_SetBase
    XML_GetBase
    XML_GetSpecifiedAttributeCount
    XML_GetIdAttributeIndex
    XML_Parse
    XML_GetBuffer
    XML_ParseBuffer
    XML_StopParser
    XML_ResumeParser
    XML_GetParsingStatus
    XML_ExternalEntityParserCreate
    XML_SetParamEntityParsing
    XML_GetErrorCode
    XML_GetCurrentLineNumber
    XML_GetCurrentColumnNumber
    XML_GetCurrentByteIndex
    XML_GetCurrentByteCount
    XML_GetInputContext
    XML_FreeContentModel
    XML_MemMalloc
    XML_MemRealloc
    XML_MemFree
    XML_ParserFree
    XML_ErrorString
    XML_ExpatVersion
    XML_GetFeatureList


;;;; done

)

;;; end of file
