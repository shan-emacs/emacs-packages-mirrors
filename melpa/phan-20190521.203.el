;;; phan.el --- Utility functions for Phan (PHP static analizer)  -*- lexical-binding: t -*-

;; Copyright (C) 2018  Friends of Emacs-PHP development

;; Author: USAMI Kenta <tadsan@pixiv.com>
;; Created: 28 Jan 2017
;; Version: 0.0.4
;; Package-Version: 20190521.203
;; Keywords: tools php
;; Package-Requires: ((emacs "24") (composer "0.0.8") (f "0.17"))
;; URL: https://github.com/emacs-php/phan.el

;; This file is NOT part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Phan is static analizer for PHP.  https://github.com/etsy/phan
;; This package has utilities and major mode for phan log format.
;;
;; # Major modes
;;
;; ## phan-log-mode
;;
;; A major mode for viewing Phan log format.
;;
;; # Commands
;;
;; ## phan-find-config-file
;;
;; Open `.phan/config.php' of current directory.
;;


;;; Code:
(require 'composer)
(require 'f)

(defgroup phan nil
  "Utilities for Phan (PHP static analizer)"
  :prefix "phan-"
  :link '(url-link :tag "Phan Wiki" "https://github.com/phan/phan/wiki")
  :link '(url-link :tag "phan.el site" "https://github.com/emacs-php/phan.el")
  :group 'tools
  :group 'php)

(defconst phan-php-types
  '("array" "bool" "float" "int" "null" "resource" "string" "void"))

(defconst phan-issues
  '(;; Issue::CATEGORY_SYNTAX
    "PhanSyntaxError"
    "PhanInvalidConstantExpression"
    "PhanInvalidNode"
    "PhanInvalidWriteToTemporaryExpression"
    "PhanInvalidTraitUse"
    "PhanContinueTargetingSwitch"
    "PhanContinueOrBreakNotInLoop"
    "PhanContinueOrBreakTooManyLevels"

    ;; Removed issues
    "PhanUnreferencedMethod"
    "PhanUnreferencedProperty"

    ;; Issue::CATEGORY_UNDEFINED
    "PhanAmbiguousTraitAliasSource"
    "PhanClassContainsAbstractMethodInternal"
    "PhanClassContainsAbstractMethod"
    "PhanEmptyFile"
    "PhanMissingRequireFile"
    "PhanInvalidRequireFile"
    "PhanParentlessClass"
    "PhanRequiredTraitNotAdded"
    "PhanTraitParentReference"
    "PhanUndeclaredAliasedMethodOfTrait"
    "PhanUndeclaredClass"
    "PhanUndeclaredClassAliasOriginal"
    "PhanUndeclaredClassCatch"
    "PhanUndeclaredClassConstant"
    "PhanUndeclaredClassInstanceof"
    "PhanUndeclaredClassMethod"
    "PhanUndeclaredClassProperty"
    "PhanUndeclaredClassReference"
    "PhanUndeclaredClassStaticProperty"
    "PhanUndeclaredClosureScope"
    "PhanUndeclaredConstant"
    "PhanUndeclaredMagicConstant"
    "PhanUndeclaredExtendedClass"
    "PhanUndeclaredFunction"
    "PhanUndeclaredInterface"
    "PhanUndeclaredMethod"
    "PhanUndeclaredProperty"
    "PhanUndeclaredStaticMethod"
    "PhanUndeclaredStaticProperty"
    "PhanUndeclaredTrait"
    "PhanUndeclaredTypeParameter"
    "PhanUndeclaredTypeReturnType"
    "PhanUndeclaredTypeProperty"
    "PhanUndeclaredTypeThrowsType"
    "PhanUndeclaredVariable"
    "PhanUndeclaredThis"
    "PhanUndeclaredVariableDim"
    "PhanUndeclaredVariableAssignOp"
    "PhanUndeclaredClassInCallable"
    "PhanUndeclaredStaticMethodInCallable"
    "PhanUndeclaredFunctionInCallable"
    "PhanUndeclaredMethodInCallable"
    "PhanUndeclaredInvokeInCallable"
    "PhanEmptyFQSENInCallable"
    "PhanInvalidFQSENInCallable"
    "PhanEmptyFQSENInClasslike"
    "PhanInvalidFQSENInClasslike"

    ;; Issue::CATEGORY_TYPE
    "PhanNonClassMethodCall"
    "PhanPossiblyNonClassMethodCall"
    "PhanTypeArrayOperator"
    "PhanTypeInvalidBitwiseBinaryOperator"
    "PhanTypeMismatchBitwiseBinaryOperands"
    "PhanTypeArraySuspicious"
    "PhanTypeArrayUnsetSuspicious"
    "PhanTypeArraySuspiciousNullable"
    "PhanTypeSuspiciousIndirectVariable"
    "PhanTypeObjectUnsetDeclaredProperty"
    "PhanTypeComparisonFromArray"
    "PhanTypeComparisonToArray"
    "PhanTypeConversionFromArray"
    "PhanTypeInstantiateAbstract"
    "PhanTypeInstantiateInterface"
    "PhanTypeInstantiateTrait"
    "PhanTypeInvalidCloneNotObject"
    "PhanTypeInvalidClosureScope"
    "PhanTypeInvalidLeftOperand"
    "PhanTypeInvalidRightOperand"
    "PhanTypeInvalidLeftOperandOfAdd"
    "PhanTypeInvalidRightOperandOfAdd"
    "PhanTypeInvalidLeftOperandOfNumericOp"
    "PhanTypeInvalidRightOperandOfNumericOp"
    "PhanTypeInvalidLeftOperandOfIntegerOp"
    "PhanTypeInvalidRightOperandOfIntegerOp"
    "PhanTypeInvalidUnaryOperandNumeric"
    "PhanTypeInvalidUnaryOperandBitwiseNot"
    "PhanTypeInvalidUnaryOperandIncOrDec"
    "PhanTypeInvalidInstanceof"
    "PhanTypeInvalidDimOffset"
    "PhanTypeInvalidDimOffsetArrayDestructuring"
    "PhanTypeInvalidExpressionArrayDestructuring"
    "PhanTypeInvalidThrowsNonObject"
    "PhanTypeInvalidThrowsNonThrowable"
    "PhanTypeInvalidThrowsIsTrait"
    "PhanTypeInvalidThrowsIsInterface"
    "PhanTypeMagicVoidWithReturn"
    "PhanTypeMismatchArgument"
    "PhanTypeMismatchArgumentNullable"
    "PhanTypeMismatchArgumentInternal"
    "PhanTypeMismatchArgumentNullableInternal"
    "PhanPartialTypeMismatchArgument"
    "PhanPartialTypeMismatchArgumentInternal"
    "PhanPossiblyNullTypeArgument"
    "PhanPossiblyNullTypeArgumentInternal"
    "PhanPossiblyFalseTypeArgument"
    "PhanPossiblyFalseTypeArgumentInternal"

    "PhanTypeMismatchDefault"
    "PhanTypeMismatchDimAssignment"
    "PhanTypeMismatchDimEmpty"
    "PhanTypeMismatchDimFetch"
    "PhanTypeMismatchDimFetchNullable"
    "PhanTypeMismatchUnpackKey"
    "PhanTypeMismatchUnpackKeyArraySpread"
    "PhanTypeMismatchUnpackValue"
    "PhanTypeMismatchArrayDestructuringKey"
    "PhanMismatchVariadicComment"
    "PhanMismatchVariadicParam"
    "PhanTypeMismatchForeach"
    "PhanTypeNoAccessiblePropertiesForeach"
    "PhanTypeNoPropertiesForeach"
    "PhanTypeSuspiciousNonTraversableForeach"
    "PhanTypeMismatchProperty"
    "PhanPossiblyNullTypeMismatchProperty"
    "PhanPossiblyFalseTypeMismatchProperty"
    "PhanPartialTypeMismatchProperty"
    "PhanTypeMismatchReturn"
    "PhanTypeMismatchReturnNullable"
    "PhanPartialTypeMismatchReturn"
    "PhanPossiblyNullTypeReturn"
    "PhanPossiblyFalseTypeReturn"
    "PhanTypeMismatchDeclaredReturn"
    "PhanTypeMismatchDeclaredReturnNullable"
    "PhanTypeMismatchDeclaredParam"
    "PhanTypeMismatchDeclaredParamNullable"
    "PhanTypeMissingReturn"
    "PhanTypeNonVarPassByRef"
    "PhanTypeParentConstructorCalled"
    "PhanTypeSuspiciousEcho"
    "PhanTypeSuspiciousStringExpression"
    "PhanTypeVoidAssignment"
    "PhanTypePossiblyInvalidCallable"
    "PhanTypeInvalidCallable"
    "PhanTypeInvalidCallableArraySize"
    "PhanTypeInvalidCallableArrayKey"
    "PhanTypeInvalidCallableObjectOfMethod"
    "PhanTypeExpectedObject"
    "PhanTypeExpectedObjectOrClassName"
    "PhanTypeExpectedObjectPropAccess"
    "PhanTypeExpectedObjectPropAccessButGotNull"
    "PhanTypeExpectedObjectStaticPropAccess"

    "PhanTypeMismatchGeneratorYieldValue"
    "PhanTypeMismatchGeneratorYieldKey"
    "PhanTypeInvalidYieldFrom"
    "PhanTypeInvalidMethodName"
    "PhanTypeInvalidStaticMethodName"
    "PhanTypeInvalidCallableMethodName"
    "PhanTypeInvalidRequire"
    "PhanTypeInvalidEval"
    "PhanRelativePathUsed"
    "PhanTypeInvalidTraitReturn"
    "PhanTypeInvalidTraitParam"
    "PhanInfiniteRecursion"
    "PhanTypeComparisonToInvalidClass"
    "PhanTypeComparisonToInvalidClassType"
    "PhanTypeInvalidPropertyName"
    "PhanTypeInvalidStaticPropertyName"
    "PhanTypeErrorInInternalCall"
    "PhanTypeInvalidPropertyDefaultReal"

    ;; Issue::CATEGORY_ANALYSIS
    "PhanUnanalyzable"
    "PhanUnanalyzableInheritance"
    "PhanInvalidConstantFQSEN"
    "PhanReservedConstantName"

    ;; Issue::CATEGORY_VARIABLE
    "PhanVariableUseClause"

    ;; Issue::CATEGORY_STATIC
    "PhanStaticCallToNonStatic"
    "PhanStaticPropIsStaticType"

    ;; Issue::CATEGORY_CONTEXT
    "PhanContextNotObject"
    "PhanContextNotObjectInCallable"
    "PhanContextNotObjectUsingSelf"

    ;; Issue::CATEGORY_DEPRECATED
    "PhanDeprecatedClass"
    "PhanDeprecatedInterface"
    "PhanDeprecatedTrait"
    "PhanDeprecatedFunction"
    "PhanDeprecatedFunctionInternal"
    "PhanDeprecatedProperty"
    "PhanDeprecatedClassConstant"
    "PhanDeprecatedCaseInsensitiveDefine"

    ;; Issue::CATEGORY_PARAMETER
    "PhanParamReqAfterOpt"
    "PhanParamSpecial1"
    "PhanParamSpecial2"
    "PhanParamSpecial3"
    "PhanParamSpecial4"
    "PhanParamSuspiciousOrder"
    "PhanParamTooFew"
    "PhanParamTooFewInternal"
    "PhanParamTooFewCallable"
    "PhanParamTooMany"
    "PhanParamTooManyUnpack"
    "PhanParamTooManyInternal"
    "PhanParamTooManyUnpackInternal"
    "PhanParamTooManyCallable"
    "PhanParamTypeMismatch"
    "PhanParamSignatureMismatch"
    "PhanParamSignatureMismatchInternal"
    "PhanParamRedefined"
    "PhanParamMustBeUserDefinedClassname"

    "PhanParamSignatureRealMismatchReturnType"
    "PhanParamSignatureRealMismatchReturnTypeInternal"
    "PhanParamSignaturePHPDocMismatchReturnType"
    "PhanParamSignatureRealMismatchTooManyRequiredParameters"
    "PhanParamSignatureRealMismatchTooManyRequiredParametersInternal"
    "PhanParamSignaturePHPDocMismatchTooManyRequiredParameters"
    "PhanParamSignatureRealMismatchTooFewParameters"
    "PhanParamSignatureRealMismatchTooFewParametersInternal"
    "PhanParamSignaturePHPDocMismatchTooFewParameters"
    "PhanParamSignatureRealMismatchHasParamType"
    "PhanParamSignatureRealMismatchHasParamTypeInternal"
    "PhanParamSignaturePHPDocMismatchHasParamType"
    "PhanParamSignatureRealMismatchHasNoParamType"
    "PhanParamSignatureRealMismatchHasNoParamTypeInternal"
    "PhanParamSignaturePHPDocMismatchHasNoParamType"
    "PhanParamSignatureRealMismatchParamIsReference"
    "PhanParamSignatureRealMismatchParamIsReferenceInternal"
    "PhanParamSignaturePHPDocMismatchParamIsReference"
    "PhanParamSignatureRealMismatchParamIsNotReference"
    "PhanParamSignatureRealMismatchParamIsNotReferenceInternal"
    "PhanParamSignaturePHPDocMismatchParamIsNotReference"
    "PhanParamSignatureRealMismatchParamVariadic"
    "PhanParamSignatureRealMismatchParamVariadicInternal"
    "PhanParamSignaturePHPDocMismatchParamVariadic"
    "PhanParamSignatureRealMismatchParamNotVariadic"
    "PhanParamSignatureRealMismatchParamNotVariadicInternal"
    "PhanParamSignaturePHPDocMismatchParamNotVariadic"
    "PhanParamSignatureRealMismatchParamType"
    "PhanParamSignatureRealMismatchParamTypeInternal"
    "PhanParamSignaturePHPDocMismatchParamType"

    ;; Issue::CATEGORY_NOOP
    "PhanNoopArray"
    "PhanNoopClosure"
    "PhanNoopConstant"
    "PhanNoopProperty"
    "PhanNoopArrayAccess"
    "PhanNoopVariable"
    "PhanNoopUnaryOperator"
    "PhanNoopBinaryOperator"
    "PhanNoopStringLiteral"
    "PhanNoopEncapsulatedStringLiteral"
    "PhanNoopNumericLiteral"
    "PhanNoopEmpty"
    "PhanNoopIsset"
    "PhanNoopCast"
    "PhanUnreachableCatch"
    "PhanUnreferencedClass"
    "PhanUnreferencedFunction"
    "PhanUnreferencedPublicMethod"
    "PhanUnreferencedProtectedMethod"
    "PhanUnreferencedPrivateMethod"
    "PhanUnreferencedPublicProperty"
    "PhanUnreferencedProtectedProperty"
    "PhanUnreferencedPrivateProperty"
    "PhanUnreferencedPHPDocProperty"
    "PhanReadOnlyPublicProperty"
    "PhanReadOnlyProtectedProperty"
    "PhanReadOnlyPrivateProperty"
    "PhanReadOnlyPHPDocProperty"
    "PhanWriteOnlyPublicProperty"
    "PhanWriteOnlyProtectedProperty"
    "PhanWriteOnlyPrivateProperty"
    "PhanWriteOnlyPHPDocProperty"
    "PhanUnreferencedConstant"
    "PhanUnreferencedPublicClassConstant"
    "PhanUnreferencedProtectedClassConstant"
    "PhanUnreferencedPrivateClassConstant"
    "PhanUnreferencedClosure"
    "PhanUnreferencedUseNormal"
    "PhanUnreferencedUseFunction"
    "PhanUnreferencedUseConstant"
    "PhanUseNormalNoEffect"
    "PhanUseNormalNamespacedNoEffect"
    "PhanUseFunctionNoEffect"
    "PhanUseConstantNoEffect"

    "PhanUnusedVariable"
    "PhanUnusedPublicMethodParameter"
    "PhanUnusedPublicFinalMethodParameter"
    "PhanUnusedPublicNoOverrideMethodParameter"
    "PhanUnusedProtectedMethodParameter"
    "PhanUnusedProtectedFinalMethodParameter"
    "PhanUnusedProtectedNoOverrideMethodParameter"
    "PhanUnusedPrivateMethodParameter"
    "PhanUnusedPrivateFinalMethodParameter"
    "PhanUnusedClosureUseVariable"
    "PhanShadowedVariableInArrowFunc"
    "PhanUnusedClosureParameter"
    "PhanUnusedGlobalFunctionParameter"
    "PhanUnusedVariableValueOfForeachWithKey"
    "PhanUnusedVariableCaughtException"
    "PhanUnusedGotoLabel"
    "PhanUnusedVariableReference"
    "PhanUnusedVariableStatic"
    "PhanUnusedVariableGlobal"
    "PhanVariableDefinitionCouldBeConstant"
    "PhanVariableDefinitionCouldBeConstantEmptyArray"
    "PhanVariableDefinitionCouldBeConstantString"
    "PhanVariableDefinitionCouldBeConstantFloat"
    "PhanVariableDefinitionCouldBeConstantInt"
    "PhanVariableDefinitionCouldBeConstantTrue"
    "PhanVariableDefinitionCouldBeConstantFalse"
    "PhanVariableDefinitionCouldBeConstantNull"

    ;; Issue::CATEGORY_REDEFINE
    "PhanRedefineClass"
    "PhanRedefineClassAlias"
    "PhanRedefineClassInternal"
    "PhanRedefineFunction"
    "PhanRedefineFunctionInternal"
    "PhanRedefineClassConstant"
    "PhanRedefineProperty"
    "PhanIncompatibleCompositionProp"
    "PhanIncompatibleCompositionMethod"
    "PhanRedefinedUsedTrait"
    "PhanRedefinedInheritedInterface"
    "PhanRedefinedExtendedClass"

    ;; Issue::CATEGORY_ACCESS
    "PhanAccessPropertyPrivate"
    "PhanAccessPropertyProtected"

    "PhanAccessReadOnlyProperty"
    "PhanAccessWriteOnlyProperty"
    "PhanAccessReadOnlyMagicProperty"
    "PhanAccessWriteOnlyMagicProperty"

    "PhanAccessMethodPrivate"
    "PhanAccessMethodPrivateWithCallMagicMethod"
    "PhanAccessMethodProtected"
    "PhanAccessMethodProtectedWithCallMagicMethod"
    "PhanAccessSignatureMismatch"
    "PhanAccessSignatureMismatchInternal"
    "PhanPropertyAccessSignatureMismatch"
    "PhanPropertyAccessSignatureMismatchInternal"
    "PhanConstantAccessSignatureMismatch"
    "PhanConstantAccessSignatureMismatchInternal"
    "PhanAccessStaticToNonStatic"
    "PhanAccessNonStaticToStatic"
    "PhanAccessStaticToNonStaticProperty"
    "PhanAccessNonStaticToStaticProperty"
    "PhanAccessClassConstantPrivate"
    "PhanAccessClassConstantProtected"
    "PhanAccessPropertyStaticAsNonStatic"
    "PhanAccessPropertyNonStaticAsStatic"
    "PhanAccessOwnConstructor"

    "PhanAccessConstantInternal"
    "PhanAccessClassInternal"
    "PhanAccessClassConstantInternal"
    "PhanAccessPropertyInternal"
    "PhanAccessMethodInternal"
    "PhanAccessWrongInheritanceCategory"
    "PhanAccessWrongInheritanceCategoryInternal"
    "PhanAccessExtendsFinalClass"
    "PhanAccessExtendsFinalClassInternal"
    "PhanAccessOverridesFinalMethod"
    "PhanAccessOverridesFinalMethodInternal"
    "PhanAccessOverridesFinalMethodPHPDoc"

    ;; Issue::CATEGORY_COMPATIBLE
    "PhanCompatibleExpressionPHP7"
    "PhanCompatiblePHP7"
    "PhanCompatibleNullableTypePHP70"
    "PhanCompatibleShortArrayAssignPHP70"
    "PhanCompatibleKeyedArrayAssignPHP70"
    "PhanCompatibleVoidTypePHP70"
    "PhanCompatibleIterableTypePHP70"
    "PhanCompatibleNullableTypePHP71"
    "PhanCompatibleUseVoidPHP70"
    "PhanCompatibleUseIterablePHP71"
    "PhanCompatibleUseObjectPHP71"
    "PhanCompatibleMultiExceptionCatchPHP70"
    "PhanCompatibleNegativeStringOffset"
    "PhanCompatibleAutoload"

    ;; Issue::CATEGORY_GENERIC
    "PhanTemplateTypeConstant"
    "PhanTemplateTypeStaticMethod"
    "PhanTemplateTypeStaticProperty"
    "PhanGenericGlobalVariable"
    "PhanGenericConstructorTypes"
    "PhanTemplateTypeNotUsedInFunctionReturn"
    "PhanTemplateTypeNotDeclaredInFunctionParams"

    ;; Issue::CATEGORY_COMMENT
    "PhanInvalidCommentForDeclarationType"
    "PhanMisspelledAnnotation"
    "PhanUnextractableAnnotation"
    "PhanUnextractableAnnotationPart"
    "PhanUnextractableAnnotationSuffix"
    "PhanUnextractableAnnotationElementName"
    "PhanCommentParamWithoutRealParam"
    "PhanCommentParamAssertionWithoutRealParam"
    "PhanCommentParamOnEmptyParamList"
    "PhanCommentOverrideOnNonOverrideMethod"
    "PhanCommentOverrideOnNonOverrideConstant"
    "PhanCommentParamOutOfOrder"
    "PhanThrowTypeAbsent"
    "PhanThrowTypeAbsentForCall"
    "PhanThrowTypeMismatch"
    "PhanThrowTypeMismatchForCall"
    "PhanCommentAmbiguousClosure"
    "PhanCommentDuplicateParam"
    "PhanCommentDuplicateMagicMethod"
    "PhanCommentDuplicateMagicProperty"
     )
  "Issue names of Phan.

https://github.com/etsy/phan/blob/master/src/Phan/Issue.php
https://github.com/etsy/phan/wiki/Issue-Types-Caught-by-Phan")

(defconst phan-log-warning-keywords
  '("can't be"
    "deprecated"
    "has no return value"
    "not found"
    "only takes"
    "should be compatible"
    "Suspicious"
    "undeclared"
    "unextractable annotation"))

(defconst phan-log-class-prefix-keywords
  '(":" "but" "class" "for" "function" "is" "method" "property" "return" "takes" "to" "type"
    "Class" "Property"))

(defconst phan-log-function-prefix-keywords
  '("Function" "Method"))

(defconst phan-log-mode-syntax-table
  (let ((table (make-syntax-table)))
    (modify-syntax-entry ?$  "_" table)
    (modify-syntax-entry ?-  "_" table)
    (modify-syntax-entry ?.  "_" table)
    (modify-syntax-entry ?/  "_" table)
    (modify-syntax-entry ?:  "." table)
    (modify-syntax-entry ?>  "." table)
    (modify-syntax-entry ?\( "_" table)
    (modify-syntax-entry ?\) "_" table)
    (modify-syntax-entry ?\\ "_" table)
    (modify-syntax-entry ?_  "_" table)
    (modify-syntax-entry ??  "_" table)
    (modify-syntax-entry ?|  "_" table)
    table))

(defconst phan-log-font-lock-keywords
  (list
   (list "^\\([^:]+\\):\\([0-9]+\\)"
         '(1 font-lock-doc-face)
         '(2 font-lock-builtin-face))
   (cons (concat "\\(?:\\`\\|[ |[]\\)\\(" (regexp-opt phan-php-types) "\\)\\(?:[ |,[]\\|$\\)")
         '(1 font-lock-type-face))
   (cons "\\[]" '(0 font-lock-type-face))
   (cons (concat " " (regexp-opt phan-issues) " ")
         '(0 font-lock-keyword-face))
   (cons (concat "\\(?:\\`\\| \\)\\(" (regexp-opt phan-log-warning-keywords) "\\)[ $,]")
         '(1 font-lock-warning-face))
   (cons (concat "\\(?:|\\|, \\| " (regexp-opt phan-log-class-prefix-keywords) " \\)"
                 (rx (group (? "\\") (+ (or "|" (syntax word) (syntax symbol))) "()")))
         '(1 font-lock-function-name-face))
   (cons (concat "\\(?:|\\|, \\| " (regexp-opt phan-log-class-prefix-keywords) " \\)"
                 (rx (group "\\" (+ (or "?" "|" "[]" (syntax word) (syntax symbol))))))
         '(1 font-lock-type-face))
   (cons (concat "\\(?:|\\|, \\| " (regexp-opt phan-log-function-prefix-keywords) " \\)"
                 (rx (group "\\" (+ (or (syntax word) (syntax symbol))))))
         '(1 font-lock-function-name-face))
   (cons " constant \\(\\(?:\\sw\\|\\s_\\)+\\)"
         '(1 font-lock-constant-face))
   (cons "\\(?:::\\|->\\)\\(\\(?:\\sw\\|\\s_\\)+()\\)"
         '(1 font-lock-function-name-face))
   (cons "::\\(\\(?:\\sw\\|\\s_\\)+\\)"
         '(1 font-lock-constant-face))
   (cons " \\(?:Argument [0-9]+\\|annotation for\\) (\\(\\(?:\\sw\\|\\s_\\)+\\))"
         '(1 font-lock-variable-name-face))
   (cons " Argument [0-9]+ (\\(\\(?:\\sw\\|\\s_\\)+\\))"
         '(1 font-lock-variable-name-face))
   (cons " Call to method \\([^\n\\][^\n ]*\\) "
         '(1 font-lock-function-name-face))
   (cons "\\(?:\\$\\|->\\)\\(\\sw\\|\\s_\\)+"
         '(0 font-lock-variable-name-face))))

;; Utility functions
(defun phan--base-dir (directory)
  "Return path to current project root in `DIRECTORY'."
  (or (locate-dominating-file directory ".phan/config.php")
      (composer--find-composer-root directory)))

;; Major modes

;;;###autoload
(define-derived-mode phan-log-mode prog-mode "Phan-Log"
  "Major mode for viewing phan formatted log."
  (setq font-lock-defaults '(phan-log-font-lock-keywords))
  (view-mode))

;;;###autoload
(add-to-list 'auto-mode-alist '("/phan.*\\.log\\'" . phan-log-mode))

;; Commands

;;;###autoload
(defun phan-find-config-file ()
  "Open Phan config file of the project."
  (interactive)
  (if (null default-directory)
      (error "A variable `default-directory' is not set")
    (find-file (f-join (phan--base-dir default-directory) ".phan/config.php"))))

(provide 'phan)
;;; phan.el ends here
