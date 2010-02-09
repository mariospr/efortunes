/*
 * 
 * Copyright (C) 2006 Mario Sánchez Prada (msanchez@igalia.com)
 * 
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of version 2 of the GNU General Public
 * License as published by the Free Software Foundation.
 * 
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * General Public License for more details.
 * 
 * You should have received a copy of the GNU General Public
 * License along with this program; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA 02111-1307, USA.
 * 
 * Author: Mario Sánchez Prada (msanchez@igalia.com)
 */

/* Messages */
var requiredMsg="Campo obligatorio";
var invalidLoginMsg="Identificador no válido";
var invalidPasswordMsg="Contraseña inválida";
var passwordsDontMatchMsg="Las contraseñas no coinciden";
var invalidPhoneMsg="Número de teléfono inválido";
var invalidAccNumberMsg="Número de cuenta inválido";
var invalidAmountMsg="Cantidad inválida";

/* Regular expressions */
var nonEmptyRegexp = /^\S+/
var loginRegexp = /^[\w|\.]+$/;
var passwordRegexp = /^[\w|\.]+$/;
var amountRegexp = /^\d+(\.\d+)?$/;
var phoneRegexp = /^(\+\d{2})?[0-9]{9}[0-9]*$/;
var accNumberRegexp = /^\d{20}$/;

/* VALIDATORS */

function regexpValidation(field,errorId,regexp,message) {
  with (field) {
    /* Check the regular expresion */
    if (regexp.test(value)) {
      document.getElementById(errorId).innerHTML="";      
      return true;     
    }
    document.getElementById(errorId).innerHTML=message;      
    return false;
  }
}

function validateRequired(field,errorId)
{
  return regexpValidation(field,errorId,nonEmptyRegexp,requiredMsg);
}

function validateLogin(field,errorId)
{
  return regexpValidation(field,errorId,loginRegexp,invalidLoginMsg);
}

function validatePassword(field,errorId)
{
  return regexpValidation(field,errorId,passwordRegexp,invalidPasswordMsg);
}

function validatePhoneNumber(field,errorId)
{
  return regexpValidation(field,errorId,phoneRegexp,invalidPhoneMsg);
}

function validateAmount(field,errorId)
{
  return regexpValidation(field,errorId,amountRegexp,invalidAmountMsg);
}

function validateAccountNumber(field,errorId)
{
  return regexpValidation(field,errorId,accNumberRegexp,invalidAccNumberMsg);
}

function validatePasswordsDontMatch(field1, field2, errorId1, errorId2)
{
  if (field1.value != field2.value) {           
    document.getElementById(errorId1).innerHTML=passwordsDontMatchMsg;      
    document.getElementById(errorId2).innerHTML="";
    return false;
  }
  document.getElementById(errorId1).innerHTML="";      
  document.getElementById(errorId2).innerHTML=""; 
  return true;
}

/* --------------------------------------------- */


/* FORMS VALIDATIONS */

/* Create new fortune form */
function validateCreateNewFortuneForm(thisform)
{
  with (thisform)
    {
      errorFound = false;

      if (validateRequired(author, "authorError")==false) {
	author.focus();
	errorFound = true;
      }

      if (validateRequired(content, "contentError")==false) {
	if (!errorFound) {
	  content.focus();
	  errorFound = true;
	}
      }

      return (!errorFound);
    }
}

/* Update fortune form */
function validateUpdateFortuneForm(thisform)
{
  with (thisform)
    {
      errorFound = false;

      if (validateRequired(author, "authorError")==false) {
	author.focus();
	errorFound = true;
      }

      if (validateRequired(content, "contentError")==false) {
	if (!errorFound) {
	  content.focus();
	  errorFound = true;
	}
      }

      return (!errorFound);
    }
}
