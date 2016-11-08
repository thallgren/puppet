" Prelude {{{1
if exists("b:current_syntax")
  finish
endif

" this file uses line continuations
let s:cpo_sav = &cpo
set cpo&vim

" Folding Config {{{1
if has("folding") && exists("puppet_fold")
  setlocal foldmethod=syntax
endif

let s:foldable_groups = split(
      \	  get(
      \	    b:,
      \	    'puppet_foldable_groups',
      \	    get(g:, 'puppet_foldable_groups', 'ALL')
      \	  )
      \	)

function! s:foldable(...) abort
  if index(s:foldable_groups, 'ALL') > -1
    return 1
  endif

  for l:i in a:000
    if index(s:foldable_groups, l:i) > -1
      return 1
    endif
  endfor

  return 0
endfunction " }}}

syn cluster puppetNotTop contains=@puppetExtendedStringSpecial,@puppetRegexpSpecial,@puppetDeclaration,puppetConditional,puppetExceptional,puppetMethodExceptional,puppetTodo

" Whitespace Errors {{{1
if exists("puppet_space_errors")
  if !exists("puppet_no_trail_space_error")
    syn match puppetSpaceError display excludenl "\s\+$"
  endif
  if !exists("puppet_no_tab_space_error")
    syn match puppetSpaceError display " \+\t"me=e-1
  endif
endif

" Operators {{{1
" if exists("puppet_operators")
  syn match  puppetOperator "[~!^|*/%+-]\|&\.\@!\|\%(class\s*\)\@<!<<\|<=>\|<=\|\%(<\|\<class\s\+\u\w*\s*\)\@<!<[^<]\@=\|===\|==\|=\~\|>>\|>=\|=\@1<!>\|\*\*\|\.\.\.\|\.\."
  syn match  puppetOperator "->\|=>\|-=\|/=\|\*\*=\|\*=\|&&=\|&=\|&&\|||=\||=\|||\|%=\|+=\|!\~\|!="
  syn region puppetBracketOperator matchgroup=puppetDelimiter start="\[\s*" end="\s*]" contains=ALLBUT,@puppetNotTop
  syn region puppetBraceOperator matchgroup=puppetDelimiter start="{\s*" end="\s*}" contains=ALLBUT,@puppetNotTop
  syn region puppetParenOperator matchgroup=puppetDelimiter start="(\s*" end="\s*)" contains=ALLBUT,@puppetNotTop
" endif

" Expression Substitution and Backslash Notation {{{1
syn match puppetStringEscape "\\\\\|\\[abefnrstv]\|\\\o\{1,3}\|\\x\x\{1,2}"						    contained display
syn match puppetStringEscape "\%(\\M-\\C-\|\\C-\\M-\|\\M-\\c\|\\c\\M-\|\\c\|\\C-\|\\M-\)\%(\\\o\{1,3}\|\\x\x\{1,2}\|\\\=\S\)" contained display
syn match puppetQuoteEscape  "\\[\\']"											    contained display

syn region puppetInterpolation	      matchgroup=puppetInterpolationDelimiter start="${" end="}" contained contains=ALLBUT,@puppetNotTop
syn match  puppetInterpolation	      "$\w\+"    display contained contains=puppetInterpolationDelimiter,puppetVariable
syn match  puppetInterpolationDelimiter "$\ze\$\w\+" display contained
syn match  puppetInterpolation	      "$\$\%(-\w\|\W\)"       display contained contains=puppetInterpolationDelimiter,puppetVariable,puppetInvalidVariable
syn match  puppetInterpolationDelimiter "$\ze\$\%(-\w\|\W\)"    display contained
syn region puppetNoInterpolation	      start="\\${" end="}"	      contained
syn match  puppetNoInterpolation	      "\\${"		      display contained
syn match  puppetNoInterpolation	      "\\$\$\W"		      display contained

syn match puppetDelimiterEscape	"\\[(<{\[)>}\]]" transparent display contained contains=NONE

syn region puppetNestedParentheses    start="("  skip="\\\\\|\\)"  matchgroup=puppetString end=")"	transparent contained
syn region puppetNestedCurlyBraces    start="{"  skip="\\\\\|\\}"  matchgroup=puppetString end="}"	transparent contained
syn region puppetNestedAngleBrackets  start="<"  skip="\\\\\|\\>"  matchgroup=puppetString end=">"	transparent contained
syn region puppetNestedSquareBrackets start="\[" skip="\\\\\|\\\]" matchgroup=puppetString end="\]"	transparent contained

" Regular Expression Metacharacters {{{1
" These are mostly Oniguruma ready
syn region puppetRegexpComment	matchgroup=puppetRegexpSpecial   start="(?#"								  skip="\\)"  end=")"  contained
syn region puppetRegexpParens	matchgroup=puppetRegexpSpecial   start="(\(?:\|?<\=[=!]\|?>\|?<[a-z_]\w*>\|?[imx]*-[imx]*:\=\|\%(?#\)\@!\)" skip="\\)"  end=")"  contained transparent contains=@puppetRegexpSpecial
syn region puppetRegexpBrackets	matchgroup=puppetRegexpCharClass start="\[\^\="								  skip="\\\]" end="\]" contained transparent contains=puppetStringEscape,puppetRegexpEscape,puppetRegexpCharClass oneline
syn match  puppetRegexpCharClass	"\\[DdHhSsWw]"	       contained display
syn match  puppetRegexpCharClass	"\[:\^\=\%(alnum\|alpha\|ascii\|blank\|cntrl\|digit\|graph\|lower\|print\|punct\|space\|upper\|xdigit\):\]" contained
syn match  puppetRegexpEscape	"\\[].*?+^$|\\/(){}[]" contained
syn match  puppetRegexpQuantifier	"[*?+][?+]\="	       contained display
syn match  puppetRegexpQuantifier	"{\d\+\%(,\d*\)\=}?\=" contained display
syn match  puppetRegexpAnchor	"[$^]\|\\[ABbGZz]"     contained display
syn match  puppetRegexpDot	"\."		       contained display
syn match  puppetRegexpSpecial	"|"		       contained display
syn match  puppetRegexpSpecial	"\\[1-9]\d\=\d\@!"     contained display
syn match  puppetRegexpSpecial	"\\k<\%([a-z_]\w*\|-\=\d\+\)\%([+-]\d\+\)\=>" contained display
syn match  puppetRegexpSpecial	"\\k'\%([a-z_]\w*\|-\=\d\+\)\%([+-]\d\+\)\='" contained display
syn match  puppetRegexpSpecial	"\\g<\%([a-z_]\w*\|-\=\d\+\)>" contained display
syn match  puppetRegexpSpecial	"\\g'\%([a-z_]\w*\|-\=\d\+\)'" contained display

syn cluster puppetStringSpecial	      contains=puppetInterpolation,puppetNoInterpolation,puppetStringEscape
syn cluster puppetExtendedStringSpecial contains=@puppetStringSpecial,puppetNestedParentheses,puppetNestedCurlyBraces,puppetNestedAngleBrackets,puppetNestedSquareBrackets
syn cluster puppetRegexpSpecial	      contains=puppetRegexpSpecial,puppetRegexpEscape,puppetRegexpBrackets,puppetRegexpCharClass,puppetRegexpDot,puppetRegexpQuantifier,puppetRegexpAnchor,puppetRegexpParens,puppetRegexpComment

syn match puppetInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[xX]\x\+\%(_\x\+\)*r\=i\=\>" display
syn match puppetInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0[dD]\)\=\%(0\|[1-9]\d*\%(_\d\+\)*\)r\=i\=\>" display
syn match puppetInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[oO]\=\o\+\%(_\o\+\)*r\=i\=\>" display
syn match puppetInteger	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<0[bB][01]\+\%(_[01]\+\)*r\=i\=\>" display
syn match puppetFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\.\d\+\%(_\d\+\)*r\=i\=\>" display
syn match puppetFloat	"\%(\%(\w\|[]})\"']\s*\)\@<!-\)\=\<\%(0\|[1-9]\d*\%(_\d\+\)*\)\%(\.\d\+\%(_\d\+\)*\)\=\%([eE][-+]\=\d\+\%(_\d\+\)*\)r\=i\=\>" display

syn match puppetVariable "$\%(::\)\=\w\+\%(::\w\+\)*" display
syn match puppetName "\%(::\)\=[a-z]\w*\%(::[a-z]\w*\)*" display
syn match puppetType "\%(::\)\=[A-Z]\w*\%(::[A-Z]\w*\)*" display
syn match puppetWord "\%(\%(::\)\=\%(_[\w-]*\w\+\)\|\%([a-z]\%(\w*-\)\+\w\+\)\)\+" display

" bad name containing combinations of segment starting with lower case and segement starting with upper case (or vice versa)
syn match puppetNameBad "\%(::\)\=\%(\w\+::\)*\%(\%([a-z]\w*::[A-Z]\w*\)\|\%([A-Z]\w*::[a-z]\w*\)\)\%(::\w\+\)*" display
syn cluster puppetNameOrType contains=puppetVariable,puppetName,puppetType,puppetWord,puppetNameBad

" Normal String {{{1
let s:spell_cluster = exists('puppet_spellcheck_strings') ? ',@Spell' : ''
exe 'syn region puppetString matchgroup=puppetStringDelimiter start="\"" end="\"" skip="\\\\\|\\\"" ' .
      \ (s:foldable('%') ? 'fold' : '') . ' contains=@puppetStringSpecial' . s:spell_cluster
exe 'syn region puppetString matchgroup=puppetStringDelimiter start="''" end="''" skip="\\\\\|\\''" ' .
      \ (s:foldable('%') ? 'fold' : '') . ' contains=puppetQuoteEscape'	 . s:spell_cluster

" Normal Regular Expression {{{1
if s:foldable('/')
  syn region puppetRegexp matchgroup=puppetRegexpDelimiter start="\%(\%(^\|\<\%(and\|or\|while\|until\|unless\|if\|elsif\|when\|not\|then\|else\)\|[;\~=!|&(,{[<>?:*+-]\)\s*\)\@<=/" end="/[iomxneus]*" skip="\\\\\|\\/" contains=@puppetRegexpSpecial fold
  syn region puppetRegexp matchgroup=puppetRegexpDelimiter start="\%(\h\k*\s\+\)\@<=/[ \t=]\@!" end="/[iomxneus]*" skip="\\\\\|\\/" contains=@puppetRegexpSpecial fold
else
  syn region puppetRegexp matchgroup=puppetRegexpDelimiter start="\%(\%(^\|\<\%(and\|or\|while\|until\|unless\|if\|elsif\|when\|not\|then\|else\)\|[;\~=!|&(,{[<>?:*+-]\)\s*\)\@<=/" end="/[iomxneus]*" skip="\\\\\|\\/" contains=@puppetRegexpSpecial
  syn region puppetRegexp matchgroup=puppetRegexpDelimiter start="\%(\h\k*\s\+\)\@<=/[ \t=]\@!" end="/[iomxneus]*" skip="\\\\\|\\/" contains=@puppetRegexpSpecial
endif

" Here Document {{{1
syn region puppetHeredocStart matchgroup=puppetStringDelimiter start=+@(\s*\%("[^"]\+"\|[^":/)[:space:]]\+\)\%(\s*:\s*[a-z]\w\+\)\=\%(\s*/\w\+\)\=\s*)+ end=+$+ oneline contains=ALLBUT,@puppetNotTop

syn region puppetString start=+@(\s*"\z([^"]\+\)"+hs=s+2  matchgroup=puppetStringDelimiter end=+^\s*|\=\s*-\=\s*\zs\z1$+ contains=puppetHeredocStart,@puppetStringSpecial keepend
syn region puppetString start=+@(\s*\z([^":/)[:space:]]\+\)+hs=s+2  matchgroup=puppetStringDelimiter end=+^\s*|\=\s*-\=\s*\zs\z1$+ contains=puppetHeredocStart		    keepend

syn match puppetControl	    "\<\%(case\|and\|or\|in\)\>"
syn match puppetKeyword	    "\<\%(class\|define\|inherits\|node\|undef\|function\|type\|attr\|private\)\>"
syn match puppetConstant	"\<\%(default\|undef\)\>"
syn match puppetConditional "\<\%(if\|else\|elsif\|unless\)\>"
syn match puppetBoolean	    "\<\%(true\|false\)\>"

syn match puppetType "\<augeas\>"
syn match puppetType "\<computer\>"
syn match puppetType "\<cron\>"
syn match puppetType "\<exec\>"
syn match puppetType "\<file\>"
syn match puppetType "\<filebucket\>"
syn match puppetType "\<group\>"
syn match puppetType "\<host\>"
syn match puppetType "\<interface\>"
syn match puppetType "\<k5login\>"
syn match puppetType "\<macauthorization\>"
syn match puppetType "\<mailalias\>"
syn match puppetType "\<maillist\>"
syn match puppetType "\<mcx\>"
syn match puppetType "\<mount\>"
syn match puppetType "\<nagios_command\>"
syn match puppetType "\<nagios_contact\>"
syn match puppetType "\<nagios_contactgroup\>"
syn match puppetType "\<nagios_host\>"
syn match puppetType "\<nagios_hostdependency\>"
syn match puppetType "\<nagios_hostescalation\>"
syn match puppetType "\<nagios_hostextinfo\>"
syn match puppetType "\<nagios_hostgroup\>"
syn match puppetType "\<nagios_service\>"
syn match puppetType "\<nagios_servicedependency\>"
syn match puppetType "\<nagios_serviceescalation\>"
syn match puppetType "\<nagios_serviceextinfo\>"
syn match puppetType "\<nagios_servicegroup\>"
syn match puppetType "\<nagios_timeperiod\>"
syn match puppetType "\<notify\>"
syn match puppetType "\<package\>"
syn match puppetType "\<resources\>"
syn match puppetType "\<router\>"
syn match puppetType "\<schedule\>"
syn match puppetType "\<scheduled_task\>"
syn match puppetType "\<selboolean\>"
syn match puppetType "\<selmodule\>"
syn match puppetType "\<service\>"
syn match puppetType "\<ssh_authorized_key\>"
syn match puppetType "\<sshkey\>"
syn match puppetType "\<stage\>"
syn match puppetType "\<tidy\>"
syn match puppetType "\<user\>"
syn match puppetType "\<vlan\>"
syn match puppetType "\<whit\>"
syn match puppetType "\<yumrepo\>"
syn match puppetType "\<zfs\>"
syn match puppetType "\<zone\>"
syn match puppetType "\<zpool\>"

" comments last overriding everything else
syn match   puppetComment       "\s*#.*$" contains=puppetTodo
syn region  puppetComment       start="/\*" end="\*/" contains=puppetTodo extend
syn keyword puppetTodo          TODO NOTE FIXME XXX BUG HACK contained

" Define the default highlighting.
command -nargs=+ HiLink hi def link <args>

HiLink puppetRegexp               puppetConstant
HiLink puppetOperator             Operator
HiLink puppetString               String
HiLink puppetWord                 String
HiLink puppetFloat                Float
HiLink puppetInteger              Number
HiLink puppetBoolean              Boolean
HiLink puppetName                 puppetIdentifier
HiLink puppetNameBad              Error
HiLink puppetVariable             puppetIdentifier
HiLink puppetIdentifier           Identifier
HiLink puppetType                 Type
HiLink puppetConditional          Conditional
HiLink puppetConstant             Constant
HiLink puppetControl              Statement
HiLink puppetKeyword              Keyword
HiLink puppetStringDelimiter      Delimiter
HiLink puppetDelimiter            Delimiter
HiLink puppetTodo                 Todo
HiLink puppetComment              Comment

delcommand HiLink

let b:current_syntax = "puppet"
