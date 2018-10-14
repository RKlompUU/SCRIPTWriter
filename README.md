# SCRIPT Writer
## Symbolic verification of Bitcoin's output scripts
![Alt text](.imgs/outputExampleA.png?raw=true "Example")

SCRIPT Writer defines a human friendly Bitcoin SCRIPT language and provides a set of functions to translate programs written in this extended language (called ESCRIPT, which is an acronym for Extended SCRIPT) to Bitcoin SCRIPT bytecode. See below for the syntax description, or visit https://vm100.cs.stir.ac.uk/~rkl/docu.php.

#### Installation
Dependency: Haskell's Stack (https://docs.haskellstack.org/en/stable/install_and_upgrade/)

In the root directory of this repository run:

&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\# If not already installed, install Happy:<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;stack install happy<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;\# Install Script Writer and all of its dependencies (other than Happy)<br/>
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;stack install


#### Applying the tool

The executable (SCRIPTWriter-exe) can be executed in any directory of this repository through Stack as follows: stack exec SCRIPTWriter-exe -- _arguments_

Call SCRIPTWriter-exe, with the custom script in stdin, and optionally passing some arguments (call the tool with first argument 'help' for information regarding arguments)

For example, if file scriptA contains a script written in the human friendly language, run the following in Bash: stack exec SCRIPTWriter-exe -- < scriptA

Some example scripts can be found in folder scripts/


#### Understanding SCRIPTWriter-exe's output

![Alt text](.imgs/outputExample.png?raw=true "Example")

(Note: this only describes the default verbosity output mode)

For every unique branch of the supplied output script, the tool will print a verdict under a distinct "--- Symbolic evaluation report of execution branch _i_" section

In this section, the following information is printed:

- The respective branch's decision points are shown (i.e. for every encountered IF operation, is its True branch traversed or is its False branch traversed?)
- The symbolic stack that the input script must supply
- The inferred constraints (on these supplied variables)
- The final resulting symbolic stack (note, at this point we have already performed an additional OP_VERIFY, thus the constraint that the resulting full execution of this branch must end with a true value on the stack is already in the inferred constraints set)

#### The custom syntax

The supported syntax is described below.\
Instructions on how to interpret the description:\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- The "*" symbol specifies a repeated parsing of 0 or more times\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- The "+" symbol specifies a repeated parsing of 1 or more times\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- The "|" specifies an or (either parses following the left hand\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;side or the right hand side)\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;- The ".." specifies a range of allowed characters.\
Any amount of whitespace is allowed between each instruction and between\
the PUSH keyword and the subsequent bytestring. Parsing starts by applying\
Start rule. Anything after "\#" on a line is treated as a comment (similar to how comments work in Bash).\


Start := Instruction*

Instruction := Push | Mnemonic | Byte\
Push := "PUSH" Bytestring  | "PUSH" Integer\
Integer := "i" Num+ | "i-" Num+\
Num := "0".."9"\
Bytestring := Byte+\
Byte := Hexadecimal Hexadecimal\
Hexadecimal := "0".."9" | "a".."z" | "A".."Z"\
Mnemonic := "OP_0" | "OP_FALSE" | "OP_PUSHDATA1" | "OP_PUSHDATA2"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_PUSHDATA4" | "OP_1NEGATE" | "OP_RESERVED" | "OP_1"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_2" | "OP_3" | "OP_4" | "OP_5"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_6" | "OP_7" | "OP_8" | "OP_9"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_10" | "OP_11" | "OP_12" | "OP_13"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_14" | "OP_15" | "OP_16" | "OP_NOP"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_VER" | "OP_IF" | "OP_NOTIF" | "OP_VERIF"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_VERNOTIF" | "OP_ELSE" | "OP_ENDIF" | "OP_VERIFY"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_RETURN" | "OP_TOALTSTACK" | "OP_FROMALTSTACK" | "OP_2DROP"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_2DUP" | "OP_3DUP" | "OP_2OVER" | "OP_2ROT"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_2SWAP" | "OP_IFDUP" | "OP_DEPTH" | "OP_DROP"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_DUP" | "OP_NIP" | "OP_OVER" | "OP_PICK"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_ROLL" | "OP_ROT" | "OP_SWAP" | "OP_TUCK"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_CAT" | "OP_SUBSTR" | "OP_LEFT" | "OP_RIGHT"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_SIZE" | "OP_INVERT" | "OP_AND" | "OP_OR"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_XOR" | "OP_EQUAL" | "OP_EQUALVERIFY" | "OP_RESERVED1"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_RESERVED2" | "OP_1ADD" | "OP_1SUB" | "OP_2MUL"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_2DIV" | "OP_NEGATE" | "OP_ABS" | "OP_NOT"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_0NOTEQUAL" | "OP_ADD" | "OP_SUB" | "OP_MUL"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_DIV" | "OP_MOD" | "OP_LSHIFT" | "OP_RSHIFT"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_BOOLAND" | "OP_BOOLOR" | "OP_NUMEQUAL" | "OP_NUMEQUALVERIFY"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_NUMNOTEQUAL" | "OP_LESSTHAN" | "OP_GREATERTHAN" | "OP_LESSTHANOREQUAL"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_GREATERTHANOREQUAL" | "OP_MIN" | "OP_MAX" | "OP_WITHIN"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_RIPEMD160" | "OP_SHA1" | "OP_SHA256" | "OP_HASH160"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_HASH256" | "OP_CODESEPARATOR" | "OP_CHECKSIG" | "OP_CHECKSIGVERIFY"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_CHECKMULTISIG" | "OP_CHECKMULTISIGVERIFY" | "OP_NOP1" | "OP_CHECKLOCKTIMEVERIFY"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_CHECKSEQUENCEVERIFY" | "OP_NOP4" | "OP_NOP5" | "OP_NOP6"\
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;| "OP_NOP7" | "OP_NOP8" | "OP_NOP9" | "OP_NOP10"
