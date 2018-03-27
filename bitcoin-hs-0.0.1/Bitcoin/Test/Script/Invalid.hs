
-- | copy-paste from @\"script_invalid.json\"@ (from the Satoshi client's source)

module Bitcoin.Test.Script.Invalid where

--------------------------------------------------------------------------------

-- NOTE: these fail currently (we do not recognize them as invalid)
-- hence not included the main test suite yet

invalid_json_notrecognized = [

  ["NOP",
  "'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb'",
  ">520 byte push"],
  ["0",
  "IF 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' ENDIF 1",
  ">520 byte push in non-executed IF branch"],
  ["1",
  "0x61616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161",
  ">201 opcodes executed. 0x61 is NOP"],
  ["0",
  "IF 0x6161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161 ENDIF 1",
  ">201 opcodes including non-executed IF branch. 0x61 is NOP"],
  ["1 2 3 4 5 0x6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f",
  "1 2 3 4 5 6 0x6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f",
  ">1,000 stack size (0x6f is 3DUP)"],
  ["1 2 3 4 5 0x6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f",
  "1 TOALTSTACK 2 TOALTSTACK 3 4 5 6 0x6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f",
  ">1,000 stack+altstack size"],
  ["NOP",
  "0 'aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 'bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb' 0x6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f6f 2DUP 0x616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161616161",
  "10,001-byte scriptPubKey"],


  ["NOP 0x01 1", "HASH160 0x14 0xda1745e9b549bd0bfa1a569971c77eba30cd5a4b EQUAL", "Tests for Script.IsPushOnly()"],
  ["NOP1 0x01 1", "HASH160 0x14 0xda1745e9b549bd0bfa1a569971c77eba30cd5a4b EQUAL"],

  ["0 0x01 0x50", "HASH160 0x14 0xece424a6bb6ddf4db592c0faed60685047a361b1 EQUAL", "OP_RESERVED in P2SH should fail"],
  ["0 0x01 VER", "HASH160 0x14 0x0f4d7845db968f2a81b530b6f3c1d6246d4c7e01 EQUAL", "OP_VER in P2SH should fail"]

  ]

--------------------------------------------------------------------------------

invalid_json = [
  ["", ""],
  ["", "NOP"],
  ["NOP", ""],
  ["NOP","NOP"],

  ["0x4c01","0x01 NOP", "PUSHDATA1 with not enough bytes"],
  ["0x4d0200ff","0x01 NOP", "PUSHDATA2 with not enough bytes"],
  ["0x4e03000000ffff","0x01 NOP", "PUSHDATA4 with not enough bytes"],

  ["1", "IF 0x50 ENDIF 1", "0x50 is reserved"],
  ["0x52", "0x5f ADD 0x60 EQUAL", "0x51 through 0x60 push 1 through 16 onto stack"],
  ["0","NOP"],
  ["1", "IF VER ELSE 1 ENDIF", "VER non-functional"],
  ["0", "IF VERIF ELSE 1 ENDIF", "VERIF illegal everywhere"],
  ["0", "IF VERNOTIF ELSE 1 ENDIF", "VERNOT illegal everywhere"],

  ["1 IF", "1 ENDIF", "IF/ENDIF can't span scriptSig/scriptPubKey"],
  ["1 IF 0 ENDIF", "1 ENDIF"],
  ["1 ELSE 0 ENDIF", "1"],
  ["0 NOTIF", "123"],

  ["0", "DUP IF ENDIF"],
  ["0", "IF 1 ENDIF"],
  ["0", "DUP IF ELSE ENDIF"],
  ["0", "IF 1 ELSE ENDIF"],
  ["0", "NOTIF ELSE 1 ENDIF"],

  ["0 1", "IF IF 1 ELSE 0 ENDIF ENDIF"],
  ["0 0", "IF IF 1 ELSE 0 ENDIF ENDIF"],
  ["1 0", "IF IF 1 ELSE 0 ENDIF ELSE IF 0 ELSE 1 ENDIF ENDIF"],
  ["0 1", "IF IF 1 ELSE 0 ENDIF ELSE IF 0 ELSE 1 ENDIF ENDIF"],

  ["0 0", "NOTIF IF 1 ELSE 0 ENDIF ENDIF"],
  ["0 1", "NOTIF IF 1 ELSE 0 ENDIF ENDIF"],
  ["1 1", "NOTIF IF 1 ELSE 0 ENDIF ELSE IF 0 ELSE 1 ENDIF ENDIF"],
  ["0 0", "NOTIF IF 1 ELSE 0 ENDIF ELSE IF 0 ELSE 1 ENDIF ENDIF"],

  ["1", "RETURN"],
  ["1", "DUP IF RETURN ENDIF"],

  ["1", "RETURN 'data'", "canonical prunable txout format"],
  ["0 IF", "RETURN ENDIF 1", "still prunable because IF/ENDIF can't span scriptSig/scriptPubKey"],

  ["0", "VERIFY 1"],
  ["1", "VERIFY"],
  ["1", "VERIFY 0"],

  ["1 TOALTSTACK", "FROMALTSTACK 1", "alt stack not shared between sig/pubkey"],

  ["IFDUP", "DEPTH 0 EQUAL"],
  ["DROP", "DEPTH 0 EQUAL"],
  ["DUP", "DEPTH 0 EQUAL"],
  ["1", "DUP 1 ADD 2 EQUALVERIFY 0 EQUAL"],
  ["NOP", "NIP"],
  ["NOP", "1 NIP"],
  ["NOP", "1 0 NIP"],
  ["NOP", "OVER 1"],
  ["1", "OVER"],
  ["0 1", "OVER DEPTH 3 EQUALVERIFY"],
  ["19 20 21", "PICK 19 EQUALVERIFY DEPTH 2 EQUAL"],
  ["NOP", "0 PICK"],
  ["1", "-1 PICK"],
  ["19 20 21", "0 PICK 20 EQUALVERIFY DEPTH 3 EQUAL"],
  ["19 20 21", "1 PICK 21 EQUALVERIFY DEPTH 3 EQUAL"],
  ["19 20 21", "2 PICK 22 EQUALVERIFY DEPTH 3 EQUAL"],
  ["NOP", "0 ROLL"],
  ["1", "-1 ROLL"],
  ["19 20 21", "0 ROLL 20 EQUALVERIFY DEPTH 2 EQUAL"],
  ["19 20 21", "1 ROLL 21 EQUALVERIFY DEPTH 2 EQUAL"],
  ["19 20 21", "2 ROLL 22 EQUALVERIFY DEPTH 2 EQUAL"],
  ["NOP", "ROT 1"],
  ["NOP", "1 ROT 1"],
  ["NOP", "1 2 ROT 1"],
  ["NOP", "0 1 2 ROT"],
  ["NOP", "SWAP 1"],
  ["1", "SWAP 1"],
  ["0 1", "SWAP 1 EQUALVERIFY"],
  ["NOP", "TUCK 1"],
  ["1", "TUCK 1"],
  ["1 0", "TUCK DEPTH 3 EQUALVERIFY SWAP 2DROP"],
  ["NOP", "2DUP 1"],
  ["1", "2DUP 1"],
  ["NOP", "3DUP 1"],
  ["1", "3DUP 1"],
  ["1 2", "3DUP 1"],
  ["NOP", "2OVER 1"],
  ["1", "2 3 2OVER 1"],
  ["NOP", "2SWAP 1"],
  ["1", "2 3 2SWAP 1"],

  ["'a' 'b'", "CAT", "CAT disabled"],
  ["'a' 'b' 0", "IF CAT ELSE 1 ENDIF", "CAT disabled"],
  ["'abc' 1 1", "SUBSTR", "SUBSTR disabled"],
  ["'abc' 1 1 0", "IF SUBSTR ELSE 1 ENDIF", "SUBSTR disabled"],
  ["'abc' 2 0", "IF LEFT ELSE 1 ENDIF", "LEFT disabled"],
  ["'abc' 2 0", "IF RIGHT ELSE 1 ENDIF", "RIGHT disabled"],

  ["NOP", "SIZE 1"],

  ["'abc'", "IF INVERT ELSE 1 ENDIF", "INVERT disabled"],
  ["1 2 0 IF AND ELSE 1 ENDIF", "NOP", "AND disabled"],
  ["1 2 0 IF OR ELSE 1 ENDIF", "NOP", "OR disabled"],
  ["1 2 0 IF XOR ELSE 1 ENDIF", "NOP", "XOR disabled"],
  ["2 0 IF 2MUL ELSE 1 ENDIF", "NOP", "2MUL disabled"],
  ["2 0 IF 2DIV ELSE 1 ENDIF", "NOP", "2DIV disabled"],
  ["2 2 0 IF MUL ELSE 1 ENDIF", "NOP", "MUL disabled"],
  ["2 2 0 IF DIV ELSE 1 ENDIF", "NOP", "DIV disabled"],
  ["2 2 0 IF MOD ELSE 1 ENDIF", "NOP", "MOD disabled"],
  ["2 2 0 IF LSHIFT ELSE 1 ENDIF", "NOP", "LSHIFT disabled"],
  ["2 2 0 IF RSHIFT ELSE 1 ENDIF", "NOP", "RSHIFT disabled"],

  ["0 1","EQUAL"],
  ["1 1 ADD", "0 EQUAL"],
  ["11 1 ADD 12 SUB", "11 EQUAL"],

  ["2147483648 0 ADD", "NOP", "arithmetic operands must be in range [-2^31...2^31] "],
  ["-2147483648 0 ADD", "NOP", "arithmetic operands must be in range [-2^31...2^31] "],
  ["2147483647 DUP ADD", "4294967294 NUMEQUAL", "NUMEQUAL must be in numeric range"],
  ["'abcdef' NOT", "0 EQUAL", "NOT is an arithmetic operand"],

  ["2 DUP MUL", "4 EQUAL", "disabled"],
  ["2 DUP DIV", "1 EQUAL", "disabled"],
  ["2 2MUL", "4 EQUAL", "disabled"],
  ["2 2DIV", "1 EQUAL", "disabled"],
  ["7 3 MOD", "1 EQUAL", "disabled"],
  ["2 2 LSHIFT", "8 EQUAL", "disabled"],
  ["2 1 RSHIFT", "1 EQUAL", "disabled"],

  ["1","NOP1 NOP2 NOP3 NOP4 NOP5 NOP6 NOP7 NOP8 NOP9 NOP10 2 EQUAL"],
  ["'NOP_1_to_10' NOP1 NOP2 NOP3 NOP4 NOP5 NOP6 NOP7 NOP8 NOP9 NOP10","'NOP_1_to_11' EQUAL"],

  ["0x50","1", "opcode 0x50 is reserved"],
  ["1", "IF 0xba ELSE 1 ENDIF", "opcodes above NOP10 invalid if executed"],
  ["1", "IF 0xbb ELSE 1 ENDIF"],
  ["1", "IF 0xbc ELSE 1 ENDIF"],
  ["1", "IF 0xbd ELSE 1 ENDIF"],
  ["1", "IF 0xbe ELSE 1 ENDIF"],
  ["1", "IF 0xbf ELSE 1 ENDIF"],
  ["1", "IF 0xc0 ELSE 1 ENDIF"],
  ["1", "IF 0xc1 ELSE 1 ENDIF"],
  ["1", "IF 0xc2 ELSE 1 ENDIF"],
  ["1", "IF 0xc3 ELSE 1 ENDIF"],
  ["1", "IF 0xc4 ELSE 1 ENDIF"],
  ["1", "IF 0xc5 ELSE 1 ENDIF"],
  ["1", "IF 0xc6 ELSE 1 ENDIF"],
  ["1", "IF 0xc7 ELSE 1 ENDIF"],
  ["1", "IF 0xc8 ELSE 1 ENDIF"],
  ["1", "IF 0xc9 ELSE 1 ENDIF"],
  ["1", "IF 0xca ELSE 1 ENDIF"],
  ["1", "IF 0xcb ELSE 1 ENDIF"],
  ["1", "IF 0xcc ELSE 1 ENDIF"],
  ["1", "IF 0xcd ELSE 1 ENDIF"],
  ["1", "IF 0xce ELSE 1 ENDIF"],
  ["1", "IF 0xcf ELSE 1 ENDIF"],
  ["1", "IF 0xd0 ELSE 1 ENDIF"],
  ["1", "IF 0xd1 ELSE 1 ENDIF"],
  ["1", "IF 0xd2 ELSE 1 ENDIF"],
  ["1", "IF 0xd3 ELSE 1 ENDIF"],
  ["1", "IF 0xd4 ELSE 1 ENDIF"],
  ["1", "IF 0xd5 ELSE 1 ENDIF"],
  ["1", "IF 0xd6 ELSE 1 ENDIF"],
  ["1", "IF 0xd7 ELSE 1 ENDIF"],
  ["1", "IF 0xd8 ELSE 1 ENDIF"],
  ["1", "IF 0xd9 ELSE 1 ENDIF"],
  ["1", "IF 0xda ELSE 1 ENDIF"],
  ["1", "IF 0xdb ELSE 1 ENDIF"],
  ["1", "IF 0xdc ELSE 1 ENDIF"],
  ["1", "IF 0xdd ELSE 1 ENDIF"],
  ["1", "IF 0xde ELSE 1 ENDIF"],
  ["1", "IF 0xdf ELSE 1 ENDIF"],
  ["1", "IF 0xe0 ELSE 1 ENDIF"],
  ["1", "IF 0xe1 ELSE 1 ENDIF"],
  ["1", "IF 0xe2 ELSE 1 ENDIF"],
  ["1", "IF 0xe3 ELSE 1 ENDIF"],
  ["1", "IF 0xe4 ELSE 1 ENDIF"],
  ["1", "IF 0xe5 ELSE 1 ENDIF"],
  ["1", "IF 0xe6 ELSE 1 ENDIF"],
  ["1", "IF 0xe7 ELSE 1 ENDIF"],
  ["1", "IF 0xe8 ELSE 1 ENDIF"],
  ["1", "IF 0xe9 ELSE 1 ENDIF"],
  ["1", "IF 0xea ELSE 1 ENDIF"],
  ["1", "IF 0xeb ELSE 1 ENDIF"],
  ["1", "IF 0xec ELSE 1 ENDIF"],
  ["1", "IF 0xed ELSE 1 ENDIF"],
  ["1", "IF 0xee ELSE 1 ENDIF"],
  ["1", "IF 0xef ELSE 1 ENDIF"],
  ["1", "IF 0xf0 ELSE 1 ENDIF"],
  ["1", "IF 0xf1 ELSE 1 ENDIF"],
  ["1", "IF 0xf2 ELSE 1 ENDIF"],
  ["1", "IF 0xf3 ELSE 1 ENDIF"],
  ["1", "IF 0xf4 ELSE 1 ENDIF"],
  ["1", "IF 0xf5 ELSE 1 ENDIF"],
  ["1", "IF 0xf6 ELSE 1 ENDIF"],
  ["1", "IF 0xf7 ELSE 1 ENDIF"],
  ["1", "IF 0xf8 ELSE 1 ENDIF"],
  ["1", "IF 0xf9 ELSE 1 ENDIF"],
  ["1", "IF 0xfa ELSE 1 ENDIF"],
  ["1", "IF 0xfb ELSE 1 ENDIF"],
  ["1", "IF 0xfc ELSE 1 ENDIF"],
  ["1", "IF 0xfd ELSE 1 ENDIF"],
  ["1", "IF 0xfe ELSE 1 ENDIF"],
  ["1", "IF 0xff ELSE 1 ENDIF"],

  ["1 IF 1 ELSE", "0xff ENDIF", "invalid because scriptSig and scriptPubKey are processed separately"],

  ["NOP", "RIPEMD160"],
  ["NOP", "SHA1"],
  ["NOP", "SHA256"],
  ["NOP", "HASH160"],
  ["NOP", "HASH256"],

  ["NOP1","NOP10"],

  ["1","VER", "OP_VER is reserved"],
  ["1","VERIF", "OP_VERIF is reserved"],
  ["1","VERNOTIF", "OP_VERNOTIF is reserved"],
  ["1","RESERVED1", "OP_RESERVED1 is reserved"],
  ["1","RESERVED2", "OP_RESERVED2 is reserved"],
  ["1","0xba", "0xba == OP_NOP10 + 1"],

  ["2147483648", "1ADD 1", "We cannot do math on 5-byte integers"],
  ["-2147483648", "1ADD 1", "Because we use a sign bit, -2147483648 is also 5 bytes"],

  ["1", "1 ENDIF", "ENDIF without IF"],
  ["1", "IF 1", "IF without ENDIF"],
  ["1 IF 1", "ENDIF", "IFs don't carry over"],

  ["NOP", "IF 1 ENDIF", "The following tests check the if(stack.size() < N) tests in each opcode"],
  ["NOP", "NOTIF 1 ENDIF", "They are here to catch copy-and-paste errors"],
  ["NOP", "VERIFY 1", "Most of them are duplicated elsewhere,"],

  ["NOP", "TOALTSTACK 1", "but, hey, more is always better, right?"],
  ["1", "FROMALTSTACK"],
  ["1", "2DROP 1"],
  ["1", "2DUP"],
  ["1 1", "3DUP"],
  ["1 1 1", "2OVER"],
  ["1 1 1 1 1", "2ROT"],
  ["1 1 1", "2SWAP"],
  ["NOP", "IFDUP 1"],
  ["NOP", "DROP 1"],
  ["NOP", "DUP 1"],
  ["1", "NIP"],
  ["1", "OVER"],
  ["1 1 1 3", "PICK"],
  ["0", "PICK 1"],
  ["1 1 1 3", "ROLL"],
  ["0", "ROLL 1"],
  ["1 1", "ROT"],
  ["1", "SWAP"],
  ["1", "TUCK"],

  ["NOP", "SIZE 1"],

  ["1", "EQUAL 1"],
  ["1", "EQUALVERIFY 1"],

  ["NOP", "1ADD 1"],
  ["NOP", "1SUB 1"],
  ["NOP", "NEGATE 1"],
  ["NOP", "ABS 1"],
  ["NOP", "NOT 1"],
  ["NOP", "0NOTEQUAL 1"],

  ["1", "ADD"],
  ["1", "SUB"],
  ["1", "BOOLAND"],
  ["1", "BOOLOR"],
  ["1", "NUMEQUAL"],
  ["1", "NUMEQUALVERIFY 1"],
  ["1", "NUMNOTEQUAL"],
  ["1", "LESSTHAN"],
  ["1", "GREATERTHAN"],
  ["1", "LESSTHANOREQUAL"],
  ["1", "GREATERTHANOREQUAL"],
  ["1", "MIN"],
  ["1", "MAX"],
  ["1 1", "WITHIN"],

  ["NOP", "RIPEMD160 1"],
  ["NOP", "SHA1 1"],
  ["NOP", "SHA256 1"],
  ["NOP", "HASH160 1"],
  ["NOP", "HASH256 1"]

  ]


