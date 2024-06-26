(cl:in-package #:cyclosis)

(define-octet-transcoder cp437-transcoder :cp437
  (#x00 #x7f #x00 #x7f)
  (#x80 #x00c7)
  (#x81 #x00fc)
  (#x82 #x00e9)
  (#x83 #x00e2)
  (#x84 #x00e4)
  (#x85 #x00e0)
  (#x86 #x00e5)
  (#x87 #x00e7)
  (#x88 #x00ea)
  (#x89 #x00eb)
  (#x8a #x00e8)
  (#x8b #x00ef)
  (#x8c #x00ee)
  (#x8d #x00ec)
  (#x8e #x00c4)
  (#x8f #x00c5)
  (#x90 #x00c9)
  (#x91 #x00e6)
  (#x92 #x00c6)
  (#x93 #x00f4)
  (#x94 #x00f6)
  (#x95 #x00f2)
  (#x96 #x00fb)
  (#x97 #x00f9)
  (#x98 #x00ff)
  (#x99 #x00d6)
  (#x9a #x00dc)
  (#x9b #x00a2)
  (#x9c #x00a3)
  (#x9d #x00a5)
  (#x9e #x20a7)
  (#x9f #x0192)
  (#xa0 #x00e1)
  (#xa1 #x00ed)
  (#xa2 #x00f3)
  (#xa3 #x00fa)
  (#xa4 #x00f1)
  (#xa5 #x00d1)
  (#xa6 #x00aa)
  (#xa7 #x00ba)
  (#xa8 #x00bf)
  (#xa9 #x2310)
  (#xaa #x00ac)
  (#xab #x00bd)
  (#xac #x00bc)
  (#xad #x00a1)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x2561)
  (#xb6 #x2562)
  (#xb7 #x2556)
  (#xb8 #x2555)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x255c)
  (#xbe #x255b)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x255e)
  (#xc7 #x255f)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x2567)
  (#xd0 #x2568)
  (#xd1 #x2564)
  (#xd2 #x2565)
  (#xd3 #x2559)
  (#xd4 #x2558)
  (#xd5 #x2552)
  (#xd6 #x2553)
  (#xd7 #x256b)
  (#xd8 #x256a)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x258c)
  (#xde #x2590)
  (#xdf #x2580)
  (#xe0 #x03b1)
  (#xe1 #x00df)
  (#xe2 #x0393)
  (#xe3 #x03c0)
  (#xe4 #x03a3)
  (#xe5 #x03c3)
  (#xe6 #x00b5)
  (#xe7 #x03c4)
  (#xe8 #x03a6)
  (#xe9 #x0398)
  (#xea #x03a9)
  (#xeb #x03b4)
  (#xec #x221e)
  (#xed #x03c6)
  (#xee #x03b5)
  (#xef #x2229)
  (#xf0 #x2261)
  (#xf1 #x00b1)
  (#xf2 #x2265)
  (#xf3 #x2264)
  (#xf4 #x2320)
  (#xf5 #x2321)
  (#xf6 #x00f7)
  (#xf7 #x2248)
  (#xf8 #x00b0)
  (#xf9 #x2219)
  (#xfa #x00b7)
  (#xfb #x221a)
  (#xfc #x207f)
  (#xfd #x00b2)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp850-transcoder :cp850
  (#x00 #x7f #x00 #x7f)
  (#x80 #x00c7)
  (#x81 #x00fc)
  (#x82 #x00e9)
  (#x83 #x00e2)
  (#x84 #x00e4)
  (#x85 #x00e0)
  (#x86 #x00e5)
  (#x87 #x00e7)
  (#x88 #x00ea)
  (#x89 #x00eb)
  (#x8a #x00e8)
  (#x8b #x00ef)
  (#x8c #x00ee)
  (#x8d #x00ec)
  (#x8e #x00c4)
  (#x8f #x00c5)
  (#x90 #x00c9)
  (#x91 #x00e6)
  (#x92 #x00c6)
  (#x93 #x00f4)
  (#x94 #x00f6)
  (#x95 #x00f2)
  (#x96 #x00fb)
  (#x97 #x00f9)
  (#x98 #x00ff)
  (#x99 #x00d6)
  (#x9a #x00dc)
  (#x9b #x00f8)
  (#x9c #x00a3)
  (#x9d #x00d8)
  (#x9e #x00d7)
  (#x9f #x0192)
  (#xa0 #x00e1)
  (#xa1 #x00ed)
  (#xa2 #x00f3)
  (#xa3 #x00fa)
  (#xa4 #x00f1)
  (#xa5 #x00d1)
  (#xa6 #x00aa)
  (#xa7 #x00ba)
  (#xa8 #x00bf)
  (#xa9 #x00ae)
  (#xaa #x00ac)
  (#xab #x00bd)
  (#xac #x00bc)
  (#xad #x00a1)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x00c1)
  (#xb6 #x00c2)
  (#xb7 #x00c0)
  (#xb8 #x00a9)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x00a2)
  (#xbe #x00a5)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x00e3)
  (#xc7 #x00c3)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x00a4)
  (#xd0 #x00f0)
  (#xd1 #x00d0)
  (#xd2 #x00ca)
  (#xd3 #x00cb)
  (#xd4 #x00c8)
  (#xd5 #x0131)
  (#xd6 #x00cd)
  (#xd7 #x00ce)
  (#xd8 #x00cf)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x00a6)
  (#xde #x00cc)
  (#xdf #x2580)
  (#xe0 #x00d3)
  (#xe1 #x00df)
  (#xe2 #x00d4)
  (#xe3 #x00d2)
  (#xe4 #x00f5)
  (#xe5 #x00d5)
  (#xe6 #x00b5)
  (#xe7 #x00fe)
  (#xe8 #x00de)
  (#xe9 #x00da)
  (#xea #x00db)
  (#xeb #x00d9)
  (#xec #x00fd)
  (#xed #x00dd)
  (#xee #x00af)
  (#xef #x00b4)
  (#xf0 #x00ad)
  (#xf1 #x00b1)
  (#xf2 #x2017)
  (#xf3 #x00be)
  (#xf4 #x00b6)
  (#xf5 #x00a7)
  (#xf6 #x00f7)
  (#xf7 #x00b8)
  (#xf8 #x00b0)
  (#xf9 #x00a8)
  (#xfa #x00b7)
  (#xfb #x00b9)
  (#xfc #x00b3)
  (#xfd #x00b2)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp852-transcoder :cp852
  (#x00 #x7f #x00 #x7f)
  (#x80 #x00c7)
  (#x81 #x00fc)
  (#x82 #x00e9)
  (#x83 #x00e2)
  (#x84 #x00e4)
  (#x85 #x016f)
  (#x86 #x0107)
  (#x87 #x00e7)
  (#x88 #x0142)
  (#x89 #x00eb)
  (#x8a #x0150)
  (#x8b #x0151)
  (#x8c #x00ee)
  (#x8d #x0179)
  (#x8e #x00c4)
  (#x8f #x0106)
  (#x90 #x00c9)
  (#x91 #x0139)
  (#x92 #x013a)
  (#x93 #x00f4)
  (#x94 #x00f6)
  (#x95 #x013d)
  (#x96 #x013e)
  (#x97 #x015a)
  (#x98 #x015b)
  (#x99 #x00d6)
  (#x9a #x00dc)
  (#x9b #x0164)
  (#x9c #x0165)
  (#x9d #x0141)
  (#x9e #x00d7)
  (#x9f #x010d)
  (#xa0 #x00e1)
  (#xa1 #x00ed)
  (#xa2 #x00f3)
  (#xa3 #x00fa)
  (#xa4 #x0104)
  (#xa5 #x0105)
  (#xa6 #x017d)
  (#xa7 #x017e)
  (#xa8 #x0118)
  (#xa9 #x0119)
  (#xaa #x00ac)
  (#xab #x017a)
  (#xac #x010c)
  (#xad #x015f)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x00c1)
  (#xb6 #x00c2)
  (#xb7 #x011a)
  (#xb8 #x015e)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x017b)
  (#xbe #x017c)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x0102)
  (#xc7 #x0103)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x00a4)
  (#xd0 #x0111)
  (#xd1 #x0110)
  (#xd2 #x010e)
  (#xd3 #x00cb)
  (#xd4 #x010f)
  (#xd5 #x0147)
  (#xd6 #x00cd)
  (#xd7 #x00ce)
  (#xd8 #x011b)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x0162)
  (#xde #x016e)
  (#xdf #x2580)
  (#xe0 #x00d3)
  (#xe1 #x00df)
  (#xe2 #x00d4)
  (#xe3 #x0143)
  (#xe4 #x0144)
  (#xe5 #x0148)
  (#xe6 #x0160)
  (#xe7 #x0161)
  (#xe8 #x0154)
  (#xe9 #x00da)
  (#xea #x0155)
  (#xeb #x0170)
  (#xec #x00fd)
  (#xed #x00dd)
  (#xee #x0163)
  (#xef #x00b4)
  (#xf0 #x00ad)
  (#xf1 #x02dd)
  (#xf2 #x02db)
  (#xf3 #x02c7)
  (#xf4 #x02d8)
  (#xf5 #x00a7)
  (#xf6 #x00f7)
  (#xf7 #x00b8)
  (#xf8 #x00b0)
  (#xf9 #x00a8)
  (#xfa #x02d9)
  (#xfb #x0171)
  (#xfc #x0158)
  (#xfd #x0159)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp855-transcoder :cp855
  (#x00 #x7f #x00 #x7f)
  (#x80 #x0452)
  (#x81 #x0402)
  (#x82 #x0453)
  (#x83 #x0403)
  (#x84 #x0451)
  (#x85 #x0401)
  (#x86 #x0454)
  (#x87 #x0404)
  (#x88 #x0455)
  (#x89 #x0405)
  (#x8a #x0456)
  (#x8b #x0406)
  (#x8c #x0457)
  (#x8d #x0407)
  (#x8e #x0458)
  (#x8f #x0408)
  (#x90 #x0459)
  (#x91 #x0409)
  (#x92 #x045a)
  (#x93 #x040a)
  (#x94 #x045b)
  (#x95 #x040b)
  (#x96 #x045c)
  (#x97 #x040c)
  (#x98 #x045e)
  (#x99 #x040e)
  (#x9a #x045f)
  (#x9b #x040f)
  (#x9c #x044e)
  (#x9d #x042e)
  (#x9e #x044a)
  (#x9f #x042a)
  (#xa0 #x0430)
  (#xa1 #x0410)
  (#xa2 #x0431)
  (#xa3 #x0411)
  (#xa4 #x0446)
  (#xa5 #x0426)
  (#xa6 #x0434)
  (#xa7 #x0414)
  (#xa8 #x0435)
  (#xa9 #x0415)
  (#xaa #x0444)
  (#xab #x0424)
  (#xac #x0433)
  (#xad #x0413)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x0445)
  (#xb6 #x0425)
  (#xb7 #x0438)
  (#xb8 #x0418)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x0439)
  (#xbe #x0419)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x043a)
  (#xc7 #x041a)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x00a4)
  (#xd0 #x043b)
  (#xd1 #x041b)
  (#xd2 #x043c)
  (#xd3 #x041c)
  (#xd4 #x043d)
  (#xd5 #x041d)
  (#xd6 #x043e)
  (#xd7 #x041e)
  (#xd8 #x043f)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x041f)
  (#xde #x044f)
  (#xdf #x2580)
  (#xe0 #x042f)
  (#xe1 #x0440)
  (#xe2 #x0420)
  (#xe3 #x0441)
  (#xe4 #x0421)
  (#xe5 #x0442)
  (#xe6 #x0422)
  (#xe7 #x0443)
  (#xe8 #x0423)
  (#xe9 #x0436)
  (#xea #x0416)
  (#xeb #x0432)
  (#xec #x0412)
  (#xed #x044c)
  (#xee #x042c)
  (#xef #x2116)
  (#xf0 #x00ad)
  (#xf1 #x044b)
  (#xf2 #x042b)
  (#xf3 #x0437)
  (#xf4 #x0417)
  (#xf5 #x0448)
  (#xf6 #x0428)
  (#xf7 #x044d)
  (#xf8 #x042d)
  (#xf9 #x0449)
  (#xfa #x0429)
  (#xfb #x0447)
  (#xfc #x0427)
  (#xfd #x00a7)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp857-transcoder :cp857
  (#x00 #x7f #x00 #x7f)
  (#x80 #x00c7)
  (#x81 #x00fc)
  (#x82 #x00e9)
  (#x83 #x00e2)
  (#x84 #x00e4)
  (#x85 #x00e0)
  (#x86 #x00e5)
  (#x87 #x00e7)
  (#x88 #x00ea)
  (#x89 #x00eb)
  (#x8a #x00e8)
  (#x8b #x00ef)
  (#x8c #x00ee)
  (#x8d #x0131)
  (#x8e #x00c4)
  (#x8f #x00c5)
  (#x90 #x00c9)
  (#x91 #x00e6)
  (#x92 #x00c6)
  (#x93 #x00f4)
  (#x94 #x00f6)
  (#x95 #x00f2)
  (#x96 #x00fb)
  (#x97 #x00f9)
  (#x98 #x0130)
  (#x99 #x00d6)
  (#x9a #x00dc)
  (#x9b #x00f8)
  (#x9c #x00a3)
  (#x9d #x00d8)
  (#x9e #x015e)
  (#x9f #x015f)
  (#xa0 #x00e1)
  (#xa1 #x00ed)
  (#xa2 #x00f3)
  (#xa3 #x00fa)
  (#xa4 #x00f1)
  (#xa5 #x00d1)
  (#xa6 #x011e)
  (#xa7 #x011f)
  (#xa8 #x00bf)
  (#xa9 #x00ae)
  (#xaa #x00ac)
  (#xab #x00bd)
  (#xac #x00bc)
  (#xad #x00a1)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x00c1)
  (#xb6 #x00c2)
  (#xb7 #x00c0)
  (#xb8 #x00a9)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x00a2)
  (#xbe #x00a5)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x00e3)
  (#xc7 #x00c3)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x00a4)
  (#xd0 #x00ba)
  (#xd1 #x00aa)
  (#xd2 #x00ca)
  (#xd3 #x00cb)
  (#xd4 #x00c8)
  (#xd6 #x00cd)
  (#xd7 #x00ce)
  (#xd8 #x00cf)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x00a6)
  (#xde #x00cc)
  (#xdf #x2580)
  (#xe0 #x00d3)
  (#xe1 #x00df)
  (#xe2 #x00d4)
  (#xe3 #x00d2)
  (#xe4 #x00f5)
  (#xe5 #x00d5)
  (#xe6 #x00b5)
  (#xe8 #x00d7)
  (#xe9 #x00da)
  (#xea #x00db)
  (#xeb #x00d9)
  (#xed #x00ff)
  (#xee #x00af)
  (#xef #x00b4)
  (#xf0 #x00ad)
  (#xf1 #x00b1)
  (#xf3 #x00be)
  (#xf4 #x00b6)
  (#xf5 #x00a7)
  (#xf6 #x00f7)
  (#xf7 #x00b8)
  (#xf8 #x00b0)
  (#xf9 #x00a8)
  (#xfa #x00b7)
  (#xfb #x00b9)
  (#xfc #x00b3)
  (#xfd #x00b2)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp860-transcoder :cp860
  (#x00 #x7f #x00 #x7f)
  (#x80 #x00c7)
  (#x81 #x00fc)
  (#x82 #x00e9)
  (#x83 #x00e2)
  (#x84 #x00e3)
  (#x85 #x00e0)
  (#x86 #x00c1)
  (#x87 #x00e7)
  (#x88 #x00ea)
  (#x89 #x00ca)
  (#x8a #x00e8)
  (#x8b #x00cd)
  (#x8c #x00d4)
  (#x8d #x00ec)
  (#x8e #x00c3)
  (#x8f #x00c2)
  (#x90 #x00c9)
  (#x91 #x00c0)
  (#x92 #x00c8)
  (#x93 #x00f4)
  (#x94 #x00f5)
  (#x95 #x00f2)
  (#x96 #x00da)
  (#x97 #x00f9)
  (#x98 #x00cc)
  (#x99 #x00d5)
  (#x9a #x00dc)
  (#x9b #x00a2)
  (#x9c #x00a3)
  (#x9d #x00d9)
  (#x9e #x20a7)
  (#x9f #x00d3)
  (#xa0 #x00e1)
  (#xa1 #x00ed)
  (#xa2 #x00f3)
  (#xa3 #x00fa)
  (#xa4 #x00f1)
  (#xa5 #x00d1)
  (#xa6 #x00aa)
  (#xa7 #x00ba)
  (#xa8 #x00bf)
  (#xa9 #x00d2)
  (#xaa #x00ac)
  (#xab #x00bd)
  (#xac #x00bc)
  (#xad #x00a1)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x2561)
  (#xb6 #x2562)
  (#xb7 #x2556)
  (#xb8 #x2555)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x255c)
  (#xbe #x255b)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x255e)
  (#xc7 #x255f)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x2567)
  (#xd0 #x2568)
  (#xd1 #x2564)
  (#xd2 #x2565)
  (#xd3 #x2559)
  (#xd4 #x2558)
  (#xd5 #x2552)
  (#xd6 #x2553)
  (#xd7 #x256b)
  (#xd8 #x256a)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x258c)
  (#xde #x2590)
  (#xdf #x2580)
  (#xe0 #x03b1)
  (#xe1 #x00df)
  (#xe2 #x0393)
  (#xe3 #x03c0)
  (#xe4 #x03a3)
  (#xe5 #x03c3)
  (#xe6 #x00b5)
  (#xe7 #x03c4)
  (#xe8 #x03a6)
  (#xe9 #x0398)
  (#xea #x03a9)
  (#xeb #x03b4)
  (#xec #x221e)
  (#xed #x03c6)
  (#xee #x03b5)
  (#xef #x2229)
  (#xf0 #x2261)
  (#xf1 #x00b1)
  (#xf2 #x2265)
  (#xf3 #x2264)
  (#xf4 #x2320)
  (#xf5 #x2321)
  (#xf6 #x00f7)
  (#xf7 #x2248)
  (#xf8 #x00b0)
  (#xf9 #x2219)
  (#xfa #x00b7)
  (#xfb #x221a)
  (#xfc #x207f)
  (#xfd #x00b2)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp861-transcoder :cp861
  (#x00 #x7f #x00 #x7f)
  (#x80 #x00c7)
  (#x81 #x00fc)
  (#x82 #x00e9)
  (#x83 #x00e2)
  (#x84 #x00e4)
  (#x85 #x00e0)
  (#x86 #x00e5)
  (#x87 #x00e7)
  (#x88 #x00ea)
  (#x89 #x00eb)
  (#x8a #x00e8)
  (#x8b #x00d0)
  (#x8c #x00f0)
  (#x8d #x00de)
  (#x8e #x00c4)
  (#x8f #x00c5)
  (#x90 #x00c9)
  (#x91 #x00e6)
  (#x92 #x00c6)
  (#x93 #x00f4)
  (#x94 #x00f6)
  (#x95 #x00fe)
  (#x96 #x00fb)
  (#x97 #x00dd)
  (#x98 #x00fd)
  (#x99 #x00d6)
  (#x9a #x00dc)
  (#x9b #x00f8)
  (#x9c #x00a3)
  (#x9d #x00d8)
  (#x9e #x20a7)
  (#x9f #x0192)
  (#xa0 #x00e1)
  (#xa1 #x00ed)
  (#xa2 #x00f3)
  (#xa3 #x00fa)
  (#xa4 #x00c1)
  (#xa5 #x00cd)
  (#xa6 #x00d3)
  (#xa7 #x00da)
  (#xa8 #x00bf)
  (#xa9 #x2310)
  (#xaa #x00ac)
  (#xab #x00bd)
  (#xac #x00bc)
  (#xad #x00a1)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x2561)
  (#xb6 #x2562)
  (#xb7 #x2556)
  (#xb8 #x2555)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x255c)
  (#xbe #x255b)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x255e)
  (#xc7 #x255f)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x2567)
  (#xd0 #x2568)
  (#xd1 #x2564)
  (#xd2 #x2565)
  (#xd3 #x2559)
  (#xd4 #x2558)
  (#xd5 #x2552)
  (#xd6 #x2553)
  (#xd7 #x256b)
  (#xd8 #x256a)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x258c)
  (#xde #x2590)
  (#xdf #x2580)
  (#xe0 #x03b1)
  (#xe1 #x00df)
  (#xe2 #x0393)
  (#xe3 #x03c0)
  (#xe4 #x03a3)
  (#xe5 #x03c3)
  (#xe6 #x00b5)
  (#xe7 #x03c4)
  (#xe8 #x03a6)
  (#xe9 #x0398)
  (#xea #x03a9)
  (#xeb #x03b4)
  (#xec #x221e)
  (#xed #x03c6)
  (#xee #x03b5)
  (#xef #x2229)
  (#xf0 #x2261)
  (#xf1 #x00b1)
  (#xf2 #x2265)
  (#xf3 #x2264)
  (#xf4 #x2320)
  (#xf5 #x2321)
  (#xf6 #x00f7)
  (#xf7 #x2248)
  (#xf8 #x00b0)
  (#xf9 #x2219)
  (#xfa #x00b7)
  (#xfb #x221a)
  (#xfc #x207f)
  (#xfd #x00b2)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp862-transcoder :cp862
  (#x00 #x7f #x00 #x7f)
  (#x80 #x05d0)
  (#x81 #x05d1)
  (#x82 #x05d2)
  (#x83 #x05d3)
  (#x84 #x05d4)
  (#x85 #x05d5)
  (#x86 #x05d6)
  (#x87 #x05d7)
  (#x88 #x05d8)
  (#x89 #x05d9)
  (#x8a #x05da)
  (#x8b #x05db)
  (#x8c #x05dc)
  (#x8d #x05dd)
  (#x8e #x05de)
  (#x8f #x05df)
  (#x90 #x05e0)
  (#x91 #x05e1)
  (#x92 #x05e2)
  (#x93 #x05e3)
  (#x94 #x05e4)
  (#x95 #x05e5)
  (#x96 #x05e6)
  (#x97 #x05e7)
  (#x98 #x05e8)
  (#x99 #x05e9)
  (#x9a #x05ea)
  (#x9b #x00a2)
  (#x9c #x00a3)
  (#x9d #x00a5)
  (#x9e #x20a7)
  (#x9f #x0192)
  (#xa0 #x00e1)
  (#xa1 #x00ed)
  (#xa2 #x00f3)
  (#xa3 #x00fa)
  (#xa4 #x00f1)
  (#xa5 #x00d1)
  (#xa6 #x00aa)
  (#xa7 #x00ba)
  (#xa8 #x00bf)
  (#xa9 #x2310)
  (#xaa #x00ac)
  (#xab #x00bd)
  (#xac #x00bc)
  (#xad #x00a1)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x2561)
  (#xb6 #x2562)
  (#xb7 #x2556)
  (#xb8 #x2555)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x255c)
  (#xbe #x255b)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x255e)
  (#xc7 #x255f)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x2567)
  (#xd0 #x2568)
  (#xd1 #x2564)
  (#xd2 #x2565)
  (#xd3 #x2559)
  (#xd4 #x2558)
  (#xd5 #x2552)
  (#xd6 #x2553)
  (#xd7 #x256b)
  (#xd8 #x256a)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x258c)
  (#xde #x2590)
  (#xdf #x2580)
  (#xe0 #x03b1)
  (#xe1 #x00df)
  (#xe2 #x0393)
  (#xe3 #x03c0)
  (#xe4 #x03a3)
  (#xe5 #x03c3)
  (#xe6 #x00b5)
  (#xe7 #x03c4)
  (#xe8 #x03a6)
  (#xe9 #x0398)
  (#xea #x03a9)
  (#xeb #x03b4)
  (#xec #x221e)
  (#xed #x03c6)
  (#xee #x03b5)
  (#xef #x2229)
  (#xf0 #x2261)
  (#xf1 #x00b1)
  (#xf2 #x2265)
  (#xf3 #x2264)
  (#xf4 #x2320)
  (#xf5 #x2321)
  (#xf6 #x00f7)
  (#xf7 #x2248)
  (#xf8 #x00b0)
  (#xf9 #x2219)
  (#xfa #x00b7)
  (#xfb #x221a)
  (#xfc #x207f)
  (#xfd #x00b2)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp863-transcoder :cp863
  (#x00 #x7f #x00 #x7f)
  (#x80 #x00c7)
  (#x81 #x00fc)
  (#x82 #x00e9)
  (#x83 #x00e2)
  (#x84 #x00c2)
  (#x85 #x00e0)
  (#x86 #x00b6)
  (#x87 #x00e7)
  (#x88 #x00ea)
  (#x89 #x00eb)
  (#x8a #x00e8)
  (#x8b #x00ef)
  (#x8c #x00ee)
  (#x8d #x2017)
  (#x8e #x00c0)
  (#x8f #x00a7)
  (#x90 #x00c9)
  (#x91 #x00c8)
  (#x92 #x00ca)
  (#x93 #x00f4)
  (#x94 #x00cb)
  (#x95 #x00cf)
  (#x96 #x00fb)
  (#x97 #x00f9)
  (#x98 #x00a4)
  (#x99 #x00d4)
  (#x9a #x00dc)
  (#x9b #x00a2)
  (#x9c #x00a3)
  (#x9d #x00d9)
  (#x9e #x00db)
  (#x9f #x0192)
  (#xa0 #x00a6)
  (#xa1 #x00b4)
  (#xa2 #x00f3)
  (#xa3 #x00fa)
  (#xa4 #x00a8)
  (#xa5 #x00b8)
  (#xa6 #x00b3)
  (#xa7 #x00af)
  (#xa8 #x00ce)
  (#xa9 #x2310)
  (#xaa #x00ac)
  (#xab #x00bd)
  (#xac #x00bc)
  (#xad #x00be)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x2561)
  (#xb6 #x2562)
  (#xb7 #x2556)
  (#xb8 #x2555)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x255c)
  (#xbe #x255b)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x255e)
  (#xc7 #x255f)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x2567)
  (#xd0 #x2568)
  (#xd1 #x2564)
  (#xd2 #x2565)
  (#xd3 #x2559)
  (#xd4 #x2558)
  (#xd5 #x2552)
  (#xd6 #x2553)
  (#xd7 #x256b)
  (#xd8 #x256a)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x258c)
  (#xde #x2590)
  (#xdf #x2580)
  (#xe0 #x03b1)
  (#xe1 #x00df)
  (#xe2 #x0393)
  (#xe3 #x03c0)
  (#xe4 #x03a3)
  (#xe5 #x03c3)
  (#xe6 #x00b5)
  (#xe7 #x03c4)
  (#xe8 #x03a6)
  (#xe9 #x0398)
  (#xea #x03a9)
  (#xeb #x03b4)
  (#xec #x221e)
  (#xed #x03c6)
  (#xee #x03b5)
  (#xef #x2229)
  (#xf0 #x2261)
  (#xf1 #x00b1)
  (#xf2 #x2265)
  (#xf3 #x2264)
  (#xf4 #x2320)
  (#xf5 #x2321)
  (#xf6 #x00f7)
  (#xf7 #x2248)
  (#xf8 #x00b0)
  (#xf9 #x2219)
  (#xfa #x00b7)
  (#xfb #x221a)
  (#xfc #x207f)
  (#xfd #x00b2)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp864-transcoder :cp864
  (#x00 #x7f #x00 #x7f)
  (#x80 #x00b0)
  (#x81 #x00b7)
  (#x82 #x2219)
  (#x83 #x221a)
  (#x84 #x2592)
  (#x85 #x2500)
  (#x86 #x2502)
  (#x87 #x253c)
  (#x88 #x2524)
  (#x89 #x252c)
  (#x8a #x251c)
  (#x8b #x2534)
  (#x8c #x2510)
  (#x8d #x250c)
  (#x8e #x2514)
  (#x8f #x2518)
  (#x90 #x03b2)
  (#x91 #x221e)
  (#x92 #x03c6)
  (#x93 #x00b1)
  (#x94 #x00bd)
  (#x95 #x00bc)
  (#x96 #x2248)
  (#x97 #x00ab)
  (#x98 #x00bb)
  (#x99 #xfef7)
  (#x9a #xfef8)
  (#x9d #xfefb)
  (#x9e #xfefc)
  (#xa1 #x00ad)
  (#xa2 #xfe82)
  (#xa5 #xfe84)
  (#xa8 #xfe8e)
  (#xa9 #xfe8f)
  (#xaa #xfe95)
  (#xab #xfe99)
  (#xac #x060c)
  (#xad #xfe9d)
  (#xae #xfea1)
  (#xaf #xfea5)
  (#xb0 #x0660)
  (#xb1 #x0661)
  (#xb2 #x0662)
  (#xb3 #x0663)
  (#xb4 #x0664)
  (#xb5 #x0665)
  (#xb6 #x0666)
  (#xb7 #x0667)
  (#xb8 #x0668)
  (#xb9 #x0669)
  (#xba #xfed1)
  (#xbb #x061b)
  (#xbc #xfeb1)
  (#xbd #xfeb5)
  (#xbe #xfeb9)
  (#xbf #x061f)
  (#xc0 #x00a2)
  (#xc1 #xfe80)
  (#xc2 #xfe81)
  (#xc3 #xfe83)
  (#xc4 #xfe85)
  (#xc5 #xfeca)
  (#xc6 #xfe8b)
  (#xc7 #xfe8d)
  (#xc8 #xfe91)
  (#xc9 #xfe93)
  (#xca #xfe97)
  (#xcb #xfe9b)
  (#xcc #xfe9f)
  (#xcd #xfea3)
  (#xce #xfea7)
  (#xcf #xfea9)
  (#xd0 #xfeab)
  (#xd1 #xfead)
  (#xd2 #xfeaf)
  (#xd3 #xfeb3)
  (#xd4 #xfeb7)
  (#xd5 #xfebb)
  (#xd6 #xfebf)
  (#xd7 #xfec1)
  (#xd8 #xfec5)
  (#xd9 #xfecb)
  (#xda #xfecf)
  (#xdb #x00a6)
  (#xdc #x00ac)
  (#xdd #x00f7)
  (#xde #x00d7)
  (#xdf #xfec9)
  (#xe0 #x0640)
  (#xe1 #xfed3)
  (#xe2 #xfed7)
  (#xe3 #xfedb)
  (#xe4 #xfedf)
  (#xe5 #xfee3)
  (#xe6 #xfee7)
  (#xe7 #xfeeb)
  (#xe8 #xfeed)
  (#xe9 #xfeef)
  (#xea #xfef3)
  (#xeb #xfebd)
  (#xec #xfecc)
  (#xed #xfece)
  (#xee #xfecd)
  (#xef #xfee1)
  (#xf0 #xfe7d)
  (#xf1 #x0651)
  (#xf2 #xfee5)
  (#xf3 #xfee9)
  (#xf4 #xfeec)
  (#xf5 #xfef0)
  (#xf6 #xfef2)
  (#xf7 #xfed0)
  (#xf8 #xfed5)
  (#xf9 #xfef5)
  (#xfa #xfef6)
  (#xfb #xfedd)
  (#xfc #xfed9)
  (#xfd #xfef1)
  (#xfe #x25a0))

(define-octet-transcoder cp865-transcoder :cp865
  (#x00 #x7f #x00 #x7f)
  (#x80 #x00c7)
  (#x81 #x00fc)
  (#x82 #x00e9)
  (#x83 #x00e2)
  (#x84 #x00e4)
  (#x85 #x00e0)
  (#x86 #x00e5)
  (#x87 #x00e7)
  (#x88 #x00ea)
  (#x89 #x00eb)
  (#x8a #x00e8)
  (#x8b #x00ef)
  (#x8c #x00ee)
  (#x8d #x00ec)
  (#x8e #x00c4)
  (#x8f #x00c5)
  (#x90 #x00c9)
  (#x91 #x00e6)
  (#x92 #x00c6)
  (#x93 #x00f4)
  (#x94 #x00f6)
  (#x95 #x00f2)
  (#x96 #x00fb)
  (#x97 #x00f9)
  (#x98 #x00ff)
  (#x99 #x00d6)
  (#x9a #x00dc)
  (#x9b #x00f8)
  (#x9c #x00a3)
  (#x9d #x00d8)
  (#x9e #x20a7)
  (#x9f #x0192)
  (#xa0 #x00e1)
  (#xa1 #x00ed)
  (#xa2 #x00f3)
  (#xa3 #x00fa)
  (#xa4 #x00f1)
  (#xa5 #x00d1)
  (#xa6 #x00aa)
  (#xa7 #x00ba)
  (#xa8 #x00bf)
  (#xa9 #x2310)
  (#xaa #x00ac)
  (#xab #x00bd)
  (#xac #x00bc)
  (#xad #x00a1)
  (#xae #x00ab)
  (#xaf #x00a4)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x2561)
  (#xb6 #x2562)
  (#xb7 #x2556)
  (#xb8 #x2555)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x255c)
  (#xbe #x255b)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x255e)
  (#xc7 #x255f)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x2567)
  (#xd0 #x2568)
  (#xd1 #x2564)
  (#xd2 #x2565)
  (#xd3 #x2559)
  (#xd4 #x2558)
  (#xd5 #x2552)
  (#xd6 #x2553)
  (#xd7 #x256b)
  (#xd8 #x256a)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x258c)
  (#xde #x2590)
  (#xdf #x2580)
  (#xe0 #x03b1)
  (#xe1 #x00df)
  (#xe2 #x0393)
  (#xe3 #x03c0)
  (#xe4 #x03a3)
  (#xe5 #x03c3)
  (#xe6 #x00b5)
  (#xe7 #x03c4)
  (#xe8 #x03a6)
  (#xe9 #x0398)
  (#xea #x03a9)
  (#xeb #x03b4)
  (#xec #x221e)
  (#xed #x03c6)
  (#xee #x03b5)
  (#xef #x2229)
  (#xf0 #x2261)
  (#xf1 #x00b1)
  (#xf2 #x2265)
  (#xf3 #x2264)
  (#xf4 #x2320)
  (#xf5 #x2321)
  (#xf6 #x00f7)
  (#xf7 #x2248)
  (#xf8 #x00b0)
  (#xf9 #x2219)
  (#xfa #x00b7)
  (#xfb #x221a)
  (#xfc #x207f)
  (#xfd #x00b2)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp866-transcoder :cp866
  (#x00 #x7f #x00 #x7f)
  (#x80 #x0410)
  (#x81 #x0411)
  (#x82 #x0412)
  (#x83 #x0413)
  (#x84 #x0414)
  (#x85 #x0415)
  (#x86 #x0416)
  (#x87 #x0417)
  (#x88 #x0418)
  (#x89 #x0419)
  (#x8a #x041a)
  (#x8b #x041b)
  (#x8c #x041c)
  (#x8d #x041d)
  (#x8e #x041e)
  (#x8f #x041f)
  (#x90 #x0420)
  (#x91 #x0421)
  (#x92 #x0422)
  (#x93 #x0423)
  (#x94 #x0424)
  (#x95 #x0425)
  (#x96 #x0426)
  (#x97 #x0427)
  (#x98 #x0428)
  (#x99 #x0429)
  (#x9a #x042a)
  (#x9b #x042b)
  (#x9c #x042c)
  (#x9d #x042d)
  (#x9e #x042e)
  (#x9f #x042f)
  (#xa0 #x0430)
  (#xa1 #x0431)
  (#xa2 #x0432)
  (#xa3 #x0433)
  (#xa4 #x0434)
  (#xa5 #x0435)
  (#xa6 #x0436)
  (#xa7 #x0437)
  (#xa8 #x0438)
  (#xa9 #x0439)
  (#xaa #x043a)
  (#xab #x043b)
  (#xac #x043c)
  (#xad #x043d)
  (#xae #x043e)
  (#xaf #x043f)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x2561)
  (#xb6 #x2562)
  (#xb7 #x2556)
  (#xb8 #x2555)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x255c)
  (#xbe #x255b)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x255e)
  (#xc7 #x255f)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x2567)
  (#xd0 #x2568)
  (#xd1 #x2564)
  (#xd2 #x2565)
  (#xd3 #x2559)
  (#xd4 #x2558)
  (#xd5 #x2552)
  (#xd6 #x2553)
  (#xd7 #x256b)
  (#xd8 #x256a)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x258c)
  (#xde #x2590)
  (#xdf #x2580)
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
  (#xf0 #x0401)
  (#xf1 #x0451)
  (#xf2 #x0404)
  (#xf3 #x0454)
  (#xf4 #x0407)
  (#xf5 #x0457)
  (#xf6 #x040e)
  (#xf7 #x045e)
  (#xf8 #x00b0)
  (#xf9 #x2219)
  (#xfa #x00b7)
  (#xfb #x221a)
  (#xfc #x2116)
  (#xfd #x00a4)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp869-transcoder :cp869
  (#x00 #x7f #x00 #x7f)
  (#x86 #x0386)
  (#x88 #x00b7)
  (#x89 #x00ac)
  (#x8a #x00a6)
  (#x8b #x2018)
  (#x8c #x2019)
  (#x8d #x0388)
  (#x8e #x2015)
  (#x8f #x0389)
  (#x90 #x038a)
  (#x91 #x03aa)
  (#x92 #x038c)
  (#x95 #x038e)
  (#x96 #x03ab)
  (#x97 #x00a9)
  (#x98 #x038f)
  (#x99 #x00b2)
  (#x9a #x00b3)
  (#x9b #x03ac)
  (#x9c #x00a3)
  (#x9d #x03ad)
  (#x9e #x03ae)
  (#x9f #x03af)
  (#xa0 #x03ca)
  (#xa1 #x0390)
  (#xa2 #x03cc)
  (#xa3 #x03cd)
  (#xa4 #x0391)
  (#xa5 #x0392)
  (#xa6 #x0393)
  (#xa7 #x0394)
  (#xa8 #x0395)
  (#xa9 #x0396)
  (#xaa #x0397)
  (#xab #x00bd)
  (#xac #x0398)
  (#xad #x0399)
  (#xae #x00ab)
  (#xaf #x00bb)
  (#xb0 #x2591)
  (#xb1 #x2592)
  (#xb2 #x2593)
  (#xb3 #x2502)
  (#xb4 #x2524)
  (#xb5 #x039a)
  (#xb6 #x039b)
  (#xb7 #x039c)
  (#xb8 #x039d)
  (#xb9 #x2563)
  (#xba #x2551)
  (#xbb #x2557)
  (#xbc #x255d)
  (#xbd #x039e)
  (#xbe #x039f)
  (#xbf #x2510)
  (#xc0 #x2514)
  (#xc1 #x2534)
  (#xc2 #x252c)
  (#xc3 #x251c)
  (#xc4 #x2500)
  (#xc5 #x253c)
  (#xc6 #x03a0)
  (#xc7 #x03a1)
  (#xc8 #x255a)
  (#xc9 #x2554)
  (#xca #x2569)
  (#xcb #x2566)
  (#xcc #x2560)
  (#xcd #x2550)
  (#xce #x256c)
  (#xcf #x03a3)
  (#xd0 #x03a4)
  (#xd1 #x03a5)
  (#xd2 #x03a6)
  (#xd3 #x03a7)
  (#xd4 #x03a8)
  (#xd5 #x03a9)
  (#xd6 #x03b1)
  (#xd7 #x03b2)
  (#xd8 #x03b3)
  (#xd9 #x2518)
  (#xda #x250c)
  (#xdb #x2588)
  (#xdc #x2584)
  (#xdd #x03b4)
  (#xde #x03b5)
  (#xdf #x2580)
  (#xe0 #x03b6)
  (#xe1 #x03b7)
  (#xe2 #x03b8)
  (#xe3 #x03b9)
  (#xe4 #x03ba)
  (#xe5 #x03bb)
  (#xe6 #x03bc)
  (#xe7 #x03bd)
  (#xe8 #x03be)
  (#xe9 #x03bf)
  (#xea #x03c0)
  (#xeb #x03c1)
  (#xec #x03c3)
  (#xed #x03c2)
  (#xee #x03c4)
  (#xef #x0384)
  (#xf0 #x00ad)
  (#xf1 #x00b1)
  (#xf2 #x03c5)
  (#xf3 #x03c6)
  (#xf4 #x03c7)
  (#xf5 #x00a7)
  (#xf6 #x03c8)
  (#xf7 #x0385)
  (#xf8 #x00b0)
  (#xf9 #x00a8)
  (#xfa #x03c9)
  (#xfb #x03cb)
  (#xfc #x03b0)
  (#xfd #x03ce)
  (#xfe #x25a0)
  (#xff #x00a0))

(define-octet-transcoder cp874-transcoder :cp874
  (#x00 #x7f #x00 #x7f)
  (#x80 #x20ac)
  (#x85 #x2026)
  (#x91 #x2018)
  (#x92 #x2019)
  (#x93 #x201c)
  (#x94 #x201d)
  (#x95 #x2022)
  (#x96 #x2013)
  (#x97 #x2014)
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
