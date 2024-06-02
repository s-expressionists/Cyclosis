(cl:in-package #:cyclosis)

(define-octet-transcoder iso-8859-1-transcoder
    (:iso-8859-1 :iso8859-1 :latin-1 :latin1)
  (#x00 #xff #x00 #xff))

(define-octet-transcoder iso-8859-2-transcoder
    (:iso-8859-2 :iso8859-2 :latin-2 :latin2)
  (#x00 #xa0 #x00 #xa0)
  (#xa1 #x0104)
  (#xa2 #x02d8)
  (#xa3 #x0141)
  (#xa5 #x013d)
  (#xa6 #x015a)
  (#xa9 #x0160)
  (#xaa #x015e)
  (#xab #x0164)
  (#xac #x0179)
  (#xae #x017d)
  (#xaf #x017b)
  (#xb1 #x0105)
  (#xb2 #x02db)
  (#xb3 #x0142)
  (#xb5 #x013e)
  (#xb6 #x015b)
  (#xb7 #x02c7)
  (#xb9 #x0161)
  (#xba #x015f)
  (#xbb #x0165)
  (#xbc #x017a)
  (#xbd #x02dd)
  (#xbe #x017e)
  (#xbf #x017c)
  (#xc0 #x0154)
  (#xc3 #x0102)
  (#xc5 #x0139)
  (#xc6 #x0106)
  (#xc8 #x010c)
  (#xca #x0118)
  (#xcc #x011a)
  (#xcf #x010e)
  (#xd0 #x0110)
  (#xd1 #x0143)
  (#xd2 #x0147)
  (#xd5 #x0150)
  (#xd8 #x0158)
  (#xd9 #x016e)
  (#xdb #x0170)
  (#xde #x0162)
  (#xe0 #x0155)
  (#xe3 #x0103)
  (#xe5 #x013a)
  (#xe6 #x0107)
  (#xe8 #x010d)
  (#xea #x0119)
  (#xec #x011b)
  (#xef #x010f)
  (#xf0 #x0111)
  (#xf1 #x0144)
  (#xf2 #x0148)
  (#xf5 #x0151)
  (#xf8 #x0159)
  (#xf9 #x016f)
  (#xfb #x0171)
  (#xfe #x0163)
  (#xff #x02d9))

(define-octet-transcoder iso-8859-3-transcoder
    (:iso-8859-3 :iso8859-3 :latin-3 :latin3)
  (#x00 #xa0 #x00 #xa0)
  (#xa1 #x0126)
  (#xa2 #x02d8)
  (#xa6 #x0124)
  (#xa9 #x0130)
  (#xaa #x015e)
  (#xab #x011e)
  (#xac #x0134)
  (#xaf #x017b)
  (#xb1 #x0127)
  (#xb6 #x0125)
  (#xb9 #x0131)
  (#xba #x015f)
  (#xbb #x011f)
  (#xbc #x0135)
  (#xbf #x017c)
  (#xc5 #x010a)
  (#xc6 #x0108)
  (#xd5 #x0120)
  (#xd8 #x011c)
  (#xdd #x016c)
  (#xde #x015c)
  (#xe5 #x010b)
  (#xe6 #x0109)
  (#xf5 #x0121)
  (#xf8 #x011d)
  (#xfd #x016d)
  (#xfe #x015d)
  (#xff #x02d9))

(define-octet-transcoder iso-8859-4-transcoder
    (:iso-8859-4 :iso8859-4 :latin-4 :latin4)
  (#x00 #xa0 #x00 #xa0)
  (#xa1 #x0104)
  (#xa2 #x0138)
  (#xa3 #x0156)
  (#xa5 #x0128)
  (#xa6 #x013b)
  (#xa9 #x0160)
  (#xaa #x0112)
  (#xab #x0122)
  (#xac #x0166)
  (#xae #x017d)
  (#xb1 #x0105)
  (#xb2 #x02db)
  (#xb3 #x0157)
  (#xb5 #x0129)
  (#xb6 #x013c)
  (#xb7 #x02c7)
  (#xb9 #x0161)
  (#xba #x0113)
  (#xbb #x0123)
  (#xbc #x0167)
  (#xbd #x014a)
  (#xbe #x017e)
  (#xbf #x014b)
  (#xc0 #x0100)
  (#xc7 #x012e)
  (#xc8 #x010c)
  (#xca #x0118)
  (#xcc #x0116)
  (#xcf #x012a)
  (#xd0 #x0110)
  (#xd1 #x0145)
  (#xd2 #x014c)
  (#xd3 #x0136)
  (#xd9 #x0172)
  (#xdd #x0168)
  (#xde #x016a)
  (#xe0 #x0101)
  (#xe7 #x012f)
  (#xe8 #x010d)
  (#xea #x0119)
  (#xec #x0117)
  (#xef #x012b)
  (#xf0 #x0111)
  (#xf1 #x0146)
  (#xf2 #x014d)
  (#xf3 #x0137)
  (#xf9 #x0173)
  (#xfd #x0169)
  (#xfe #x016b)
  (#xff #x02d9))

(define-octet-transcoder iso-8859-5-transcoder
    (:iso-8859-5 :cyrillic)
  (#x00 #xa0 #x00 #xa0)
  (#xa1 #x0401)
  (#xa2 #x0402)
  (#xa3 #x0403)
  (#xa4 #x0404)
  (#xa5 #x0405)
  (#xa6 #x0406)
  (#xa7 #x0407)
  (#xa8 #x0408)
  (#xa9 #x0409)
  (#xaa #x040a)
  (#xab #x040b)
  (#xac #x040c)
  (#xae #x040e)
  (#xaf #x040f)
  (#xb0 #x0410)
  (#xb1 #x0411)
  (#xb2 #x0412)
  (#xb3 #x0413)
  (#xb4 #x0414)
  (#xb5 #x0415)
  (#xb6 #x0416)
  (#xb7 #x0417)
  (#xb8 #x0418)
  (#xb9 #x0419)
  (#xba #x041a)
  (#xbb #x041b)
  (#xbc #x041c)
  (#xbd #x041d)
  (#xbe #x041e)
  (#xbf #x041f)
  (#xc0 #x0420)
  (#xc1 #x0421)
  (#xc2 #x0422)
  (#xc3 #x0423)
  (#xc4 #x0424)
  (#xc5 #x0425)
  (#xc6 #x0426)
  (#xc7 #x0427)
  (#xc8 #x0428)
  (#xc9 #x0429)
  (#xca #x042a)
  (#xcb #x042b)
  (#xcc #x042c)
  (#xcd #x042d)
  (#xce #x042e)
  (#xcf #x042f)
  (#xd0 #x0430)
  (#xd1 #x0431)
  (#xd2 #x0432)
  (#xd3 #x0433)
  (#xd4 #x0434)
  (#xd5 #x0435)
  (#xd6 #x0436)
  (#xd7 #x0437)
  (#xd8 #x0438)
  (#xd9 #x0439)
  (#xda #x043a)
  (#xdb #x043b)
  (#xdc #x043c)
  (#xdd #x043d)
  (#xde #x043e)
  (#xdf #x043f)
  (#xe0 #x0440)
  (#xe1 #x0441)
  (#xe2 #x0442)
  (#xe3 #x0443)
  (#xe4 #x0444)
  (#xe5 #x0445)
  (#xe6 #x0446)
  (#xe7 #x0447)
  (#xe8 #x0448)
  (#xe9 #x0449)
  (#xea #x044a)
  (#xeb #x044b)
  (#xec #x044c)
  (#xed #x044d)
  (#xee #x044e)
  (#xef #x044f)
  (#xf0 #x2116)
  (#xf1 #x0451)
  (#xf2 #x0452)
  (#xf3 #x0453)
  (#xf4 #x0454)
  (#xf5 #x0455)
  (#xf6 #x0456)
  (#xf7 #x0457)
  (#xf8 #x0458)
  (#xf9 #x0459)
  (#xfa #x045a)
  (#xfb #x045b)
  (#xfc #x045c)
  (#xfd #x00a7)
  (#xfe #x045e)
  (#xff #x045f))

(define-octet-transcoder iso-8859-6-transcoder
    (:iso-8859-6 :arabic)
  (#x00 #xa0 #x00 #xa0)
  (#xac #x060c)
  (#xbb #x061b)
  (#xbf #x061f)
  (#xc1 #x0621)
  (#xc2 #x0622)
  (#xc3 #x0623)
  (#xc4 #x0624)
  (#xc5 #x0625)
  (#xc6 #x0626)
  (#xc7 #x0627)
  (#xc8 #x0628)
  (#xc9 #x0629)
  (#xca #x062a)
  (#xcb #x062b)
  (#xcc #x062c)
  (#xcd #x062d)
  (#xce #x062e)
  (#xcf #x062f)
  (#xd0 #x0630)
  (#xd1 #x0631)
  (#xd2 #x0632)
  (#xd3 #x0633)
  (#xd4 #x0634)
  (#xd5 #x0635)
  (#xd6 #x0636)
  (#xd7 #x0637)
  (#xd8 #x0638)
  (#xd9 #x0639)
  (#xda #x063a)
  (#xe0 #x0640)
  (#xe1 #x0641)
  (#xe2 #x0642)
  (#xe3 #x0643)
  (#xe4 #x0644)
  (#xe5 #x0645)
  (#xe6 #x0646)
  (#xe7 #x0647)
  (#xe8 #x0648)
  (#xe9 #x0649)
  (#xea #x064a)
  (#xeb #x064b)
  (#xec #x064c)
  (#xed #x064d)
  (#xee #x064e)
  (#xef #x064f)
  (#xf0 #x0650)
  (#xf1 #x0651)
  (#xf2 #x0652))

(define-octet-transcoder iso-8859-7-transcoder
    (:iso-8859-7 :greek)
  (#x00 #xa0 #x00 #xa0)
  (#xa1 #x02bd)
  (#xa2 #x02bc)
  (#xaf #x2015)
  (#xb4 #x0384)
  (#xb5 #x0385)
  (#xb6 #x0386)
  (#xb8 #x0388)
  (#xb9 #x0389)
  (#xba #x038a)
  (#xbc #x038c)
  (#xbe #x038e)
  (#xbf #x038f)
  (#xc0 #x0390)
  (#xc1 #x0391)
  (#xc2 #x0392)
  (#xc3 #x0393)
  (#xc4 #x0394)
  (#xc5 #x0395)
  (#xc6 #x0396)
  (#xc7 #x0397)
  (#xc8 #x0398)
  (#xc9 #x0399)
  (#xca #x039a)
  (#xcb #x039b)
  (#xcc #x039c)
  (#xcd #x039d)
  (#xce #x039e)
  (#xcf #x039f)
  (#xd0 #x03a0)
  (#xd1 #x03a1)
  (#xd3 #x03a3)
  (#xd4 #x03a4)
  (#xd5 #x03a5)
  (#xd6 #x03a6)
  (#xd7 #x03a7)
  (#xd8 #x03a8)
  (#xd9 #x03a9)
  (#xda #x03aa)
  (#xdb #x03ab)
  (#xdc #x03ac)
  (#xdd #x03ad)
  (#xde #x03ae)
  (#xdf #x03af)
  (#xe0 #x03b0)
  (#xe1 #x03b1)
  (#xe2 #x03b2)
  (#xe3 #x03b3)
  (#xe4 #x03b4)
  (#xe5 #x03b5)
  (#xe6 #x03b6)
  (#xe7 #x03b7)
  (#xe8 #x03b8)
  (#xe9 #x03b9)
  (#xea #x03ba)
  (#xeb #x03bb)
  (#xec #x03bc)
  (#xed #x03bd)
  (#xee #x03be)
  (#xef #x03bf)
  (#xf0 #x03c0)
  (#xf1 #x03c1)
  (#xf2 #x03c2)
  (#xf3 #x03c3)
  (#xf4 #x03c4)
  (#xf5 #x03c5)
  (#xf6 #x03c6)
  (#xf7 #x03c7)
  (#xf8 #x03c8)
  (#xf9 #x03c9)
  (#xfa #x03ca)
  (#xfb #x03cb)
  (#xfc #x03cc)
  (#xfd #x03cd)
  (#xfe #x03ce))

(define-octet-transcoder iso-8859-8-transcoder
    (:iso-8859-8 :hebrew)
  (#x00 #xa0 #x00 #xa0)
  (#xaa #x00d7)
  (#xaf #x203e)
  (#xba #x00f7)
  (#xdf #x2017)
  (#xe0 #x05d0)
  (#xe1 #x05d1)
  (#xe2 #x05d2)
  (#xe3 #x05d3)
  (#xe4 #x05d4)
  (#xe5 #x05d5)
  (#xe6 #x05d6)
  (#xe7 #x05d7)
  (#xe8 #x05d8)
  (#xe9 #x05d9)
  (#xea #x05da)
  (#xeb #x05db)
  (#xec #x05dc)
  (#xed #x05dd)
  (#xee #x05de)
  (#xef #x05df)
  (#xf0 #x05e0)
  (#xf1 #x05e1)
  (#xf2 #x05e2)
  (#xf3 #x05e3)
  (#xf4 #x05e4)
  (#xf5 #x05e5)
  (#xf6 #x05e6)
  (#xf7 #x05e7)
  (#xf8 #x05e8)
  (#xf9 #x05e9)
  (#xfa #x05ea))

(define-octet-transcoder iso-8859-9-transcoder
    (:iso-8859-9 :iso8859-9 :latin-5 :latin5)
  (#x00 #xa0 #x00 #xa0)
  (#xd0 #x011e)
  (#xdd #x0130)
  (#xde #x015e)
  (#xf0 #x011f)
  (#xfd #x0131)
  (#xfe #x015f))

(define-octet-transcoder iso-8859-10-transcoder
    (:iso-8859-10 :iso8859-10 :latin- :latin6)
  (#x00 #xa0 #x00 #xa0)
  (#xa1 #x0104)
  (#xa2 #x0112)
  (#xa3 #x0122)
  (#xa4 #x012a)
  (#xa5 #x0128)
  (#xa6 #x0136)
  (#xa8 #x013b)
  (#xa9 #x0110)
  (#xaa #x0160)
  (#xab #x0166)
  (#xac #x017d)
  (#xae #x016a)
  (#xaf #x014a)
  (#xb1 #x0105)
  (#xb2 #x0113)
  (#xb3 #x0123)
  (#xb4 #x012b)
  (#xb5 #x0129)
  (#xb6 #x0137)
  (#xb8 #x013c)
  (#xb9 #x0111)
  (#xba #x0161)
  (#xbb #x0167)
  (#xbc #x017e)
  (#xbd #x2015)
  (#xbe #x016b)
  (#xbf #x014b)
  (#xc0 #x0100)
  (#xc7 #x012e)
  (#xc8 #x010c)
  (#xca #x0118)
  (#xcc #x0116)
  (#xd1 #x0145)
  (#xd2 #x014c)
  (#xd7 #x0168)
  (#xd9 #x0172)
  (#xe0 #x0101)
  (#xe7 #x012f)
  (#xe8 #x010d)
  (#xea #x0119)
  (#xec #x0117)
  (#xf1 #x0146)
  (#xf2 #x014d)
  (#xf7 #x0169)
  (#xf9 #x0173)
  (#xff #x0138))

(define-octet-transcoder iso-8859-11-transcoder
    (:iso-8859-11 :thai)
  (#x00 #xa0 #x00 #xa0)
  (#xa1 #x0e01)
  (#xa2 #x0e02)
  (#xa3 #x0e03)
  (#xa4 #x0e04)
  (#xa5 #x0e05)
  (#xa6 #x0e06)
  (#xa7 #x0e07)
  (#xa8 #x0e08)
  (#xa9 #x0e09)
  (#xaa #x0e0a)
  (#xab #x0e0b)
  (#xac #x0e0c)
  (#xad #x0e0d)
  (#xae #x0e0e)
  (#xaf #x0e0f)
  (#xb0 #x0e10)
  (#xb1 #x0e11)
  (#xb2 #x0e12)
  (#xb3 #x0e13)
  (#xb4 #x0e14)
  (#xb5 #x0e15)
  (#xb6 #x0e16)
  (#xb7 #x0e17)
  (#xb8 #x0e18)
  (#xb9 #x0e19)
  (#xba #x0e1a)
  (#xbb #x0e1b)
  (#xbc #x0e1c)
  (#xbd #x0e1d)
  (#xbe #x0e1e)
  (#xbf #x0e1f)
  (#xc0 #x0e20)
  (#xc1 #x0e21)
  (#xc2 #x0e22)
  (#xc3 #x0e23)
  (#xc4 #x0e24)
  (#xc5 #x0e25)
  (#xc6 #x0e26)
  (#xc7 #x0e27)
  (#xc8 #x0e28)
  (#xc9 #x0e29)
  (#xca #x0e2a)
  (#xcb #x0e2b)
  (#xcc #x0e2c)
  (#xcd #x0e2d)
  (#xce #x0e2e)
  (#xcf #x0e2f)
  (#xd0 #x0e30)
  (#xd1 #x0e31)
  (#xd2 #x0e32)
  (#xd3 #x0e33)
  (#xd4 #x0e34)
  (#xd5 #x0e35)
  (#xd6 #x0e36)
  (#xd7 #x0e37)
  (#xd8 #x0e38)
  (#xd9 #x0e39)
  (#xda #x0e3a)
  (#xdf #x0e3f)
  (#xe0 #x0e40)
  (#xe1 #x0e41)
  (#xe2 #x0e42)
  (#xe3 #x0e43)
  (#xe4 #x0e44)
  (#xe5 #x0e45)
  (#xe6 #x0e46)
  (#xe7 #x0e47)
  (#xe8 #x0e48)
  (#xe9 #x0e49)
  (#xea #x0e4a)
  (#xeb #x0e4b)
  (#xec #x0e4c)
  (#xed #x0e4d)
  (#xee #x0e4e)
  (#xef #x0e4f)
  (#xf0 #x0e50)
  (#xf1 #x0e51)
  (#xf2 #x0e52)
  (#xf3 #x0e53)
  (#xf4 #x0e54)
  (#xf5 #x0e55)
  (#xf6 #x0e56)
  (#xf7 #x0e57)
  (#xf8 #x0e58)
  (#xf9 #x0e59)
  (#xfa #x0e5a)
  (#xfb #x0e5b))

(define-octet-transcoder iso-8859-13-transcoder
    (:iso-8859-13 :iso8859-13 :latin-7 :latin7)
  (#x00 #xa0 #x00 #xa0)
  (#xa1 #x201d)
  (#xa5 #x201e)
  (#xa8 #x00d8)
  (#xaa #x0156)
  (#xaf #x00c6)
  (#xb4 #x201c)
  (#xb8 #x00f8)
  (#xba #x0157)
  (#xbf #x00e6)
  (#xc0 #x0104)
  (#xc1 #x012e)
  (#xc2 #x0100)
  (#xc3 #x0106)
  (#xc6 #x0118)
  (#xc7 #x0112)
  (#xc8 #x010c)
  (#xca #x0179)
  (#xcb #x0116)
  (#xcc #x0122)
  (#xcd #x0136)
  (#xce #x012a)
  (#xcf #x013b)
  (#xd0 #x0160)
  (#xd1 #x0143)
  (#xd2 #x0145)
  (#xd4 #x014c)
  (#xd8 #x0172)
  (#xd9 #x0141)
  (#xda #x015a)
  (#xdb #x016a)
  (#xdd #x017b)
  (#xde #x017d)
  (#xe0 #x0105)
  (#xe1 #x012f)
  (#xe2 #x0101)
  (#xe3 #x0107)
  (#xe6 #x0119)
  (#xe7 #x0113)
  (#xe8 #x010d)
  (#xea #x017a)
  (#xeb #x0117)
  (#xec #x0123)
  (#xed #x0137)
  (#xee #x012b)
  (#xef #x013c)
  (#xf0 #x0161)
  (#xf1 #x0144)
  (#xf2 #x0146)
  (#xf4 #x014d)
  (#xf8 #x0173)
  (#xf9 #x0142)
  (#xfa #x015b)
  (#xfb #x016b)
  (#xfd #x017c)
  (#xfe #x017e)
  (#xff #x2019))

(define-octet-transcoder iso-8859-14-transcoder
    (:iso-8859-14 :iso8859-14 :latin-8 :latin8)
  (#x00 #xa0 #x00 #xa0)
  (#xa1 #x1e02)
  (#xa2 #x1e03)
  (#xa4 #x010a)
  (#xa5 #x010b)
  (#xa6 #x1e0a)
  (#xa8 #x1e80)
  (#xaa #x1e82)
  (#xab #x1e0b)
  (#xac #x1ef2)
  (#xaf #x0178)
  (#xb0 #x1e1e)
  (#xb1 #x1e1f)
  (#xb2 #x0120)
  (#xb3 #x0121)
  (#xb4 #x1e40)
  (#xb5 #x1e41)
  (#xb7 #x1e56)
  (#xb8 #x1e81)
  (#xb9 #x1e57)
  (#xba #x1e83)
  (#xbb #x1e60)
  (#xbc #x1ef3)
  (#xbd #x1e84)
  (#xbe #x1e85)
  (#xbf #x1e61)
  (#xd0 #x0174)
  (#xd7 #x1e6a)
  (#xde #x0176)
  (#xf0 #x0175)
  (#xf7 #x1e6b)
  (#xfe #x0177))

(define-octet-transcoder iso-8859-15-transcoder
    (:iso-8859-15 :iso8859-15 :latin-9 :latin9)
  (#x00 #xa0 #x00 #xa0)
  (#xa4 #x20ac)
  (#xa6 #x0160)
  (#xa8 #x0161)
  (#xb4 #x017d)
  (#xb8 #x017e)
  (#xbc #x0152)
  (#xbd #x0153)
  (#xbe #x0178))