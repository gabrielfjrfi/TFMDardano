/* This file was generated automatically by the Snowball to ISO C compiler */
/* http://snowballstem.org/ */

#include "header.h"

#ifdef __cplusplus
extern "C" {
#endif
extern int stem(struct SN_env * z);
#ifdef __cplusplus
}
#endif
static int r_residual_suffix(struct SN_env * z);
static int r_verb_suffix(struct SN_env * z);
static int r_standard_suffix(struct SN_env * z);
static int r_attached_pronoun(struct SN_env * z);
static int r_R2(struct SN_env * z);
static int r_R1(struct SN_env * z);
static int r_mark_regions(struct SN_env * z);
static int r_cleaning(struct SN_env * z);
#ifdef __cplusplus
extern "C" {
#endif


extern struct SN_env * create_env(void);
extern void close_env(struct SN_env * z);


#ifdef __cplusplus
}
#endif
static const symbol s_0_1[1] = { 0xB7 };
static const symbol s_0_2[1] = { 0xE0 };
static const symbol s_0_3[1] = { 0xE1 };
static const symbol s_0_4[1] = { 0xE8 };
static const symbol s_0_5[1] = { 0xE9 };
static const symbol s_0_6[1] = { 0xEC };
static const symbol s_0_7[1] = { 0xED };
static const symbol s_0_8[1] = { 0xEF };
static const symbol s_0_9[1] = { 0xF2 };
static const symbol s_0_10[1] = { 0xF3 };
static const symbol s_0_11[1] = { 0xFA };
static const symbol s_0_12[1] = { 0xFC };

static const struct among a_0[13] =
{
/*  0 */ { 0, 0, -1, 13, 0},
/*  1 */ { 1, s_0_1, 0, 12, 0},
/*  2 */ { 1, s_0_2, 0, 2, 0},
/*  3 */ { 1, s_0_3, 0, 1, 0},
/*  4 */ { 1, s_0_4, 0, 4, 0},
/*  5 */ { 1, s_0_5, 0, 3, 0},
/*  6 */ { 1, s_0_6, 0, 6, 0},
/*  7 */ { 1, s_0_7, 0, 5, 0},
/*  8 */ { 1, s_0_8, 0, 11, 0},
/*  9 */ { 1, s_0_9, 0, 8, 0},
/* 10 */ { 1, s_0_10, 0, 7, 0},
/* 11 */ { 1, s_0_11, 0, 9, 0},
/* 12 */ { 1, s_0_12, 0, 10, 0}
};

static const symbol s_1_0[2] = { 'l', 'a' };
static const symbol s_1_1[3] = { '-', 'l', 'a' };
static const symbol s_1_2[4] = { 's', 'e', 'l', 'a' };
static const symbol s_1_3[2] = { 'l', 'e' };
static const symbol s_1_4[2] = { 'm', 'e' };
static const symbol s_1_5[3] = { '-', 'm', 'e' };
static const symbol s_1_6[2] = { 's', 'e' };
static const symbol s_1_7[3] = { '-', 't', 'e' };
static const symbol s_1_8[2] = { 'h', 'i' };
static const symbol s_1_9[3] = { '\'', 'h', 'i' };
static const symbol s_1_10[2] = { 'l', 'i' };
static const symbol s_1_11[3] = { '-', 'l', 'i' };
static const symbol s_1_12[2] = { '\'', 'l' };
static const symbol s_1_13[2] = { '\'', 'm' };
static const symbol s_1_14[2] = { '-', 'm' };
static const symbol s_1_15[2] = { '\'', 'n' };
static const symbol s_1_16[2] = { '-', 'n' };
static const symbol s_1_17[2] = { 'h', 'o' };
static const symbol s_1_18[3] = { '\'', 'h', 'o' };
static const symbol s_1_19[2] = { 'l', 'o' };
static const symbol s_1_20[4] = { 's', 'e', 'l', 'o' };
static const symbol s_1_21[2] = { '\'', 's' };
static const symbol s_1_22[3] = { 'l', 'a', 's' };
static const symbol s_1_23[5] = { 's', 'e', 'l', 'a', 's' };
static const symbol s_1_24[3] = { 'l', 'e', 's' };
static const symbol s_1_25[4] = { '-', 'l', 'e', 's' };
static const symbol s_1_26[3] = { '\'', 'l', 's' };
static const symbol s_1_27[3] = { '-', 'l', 's' };
static const symbol s_1_28[3] = { '\'', 'n', 's' };
static const symbol s_1_29[3] = { '-', 'n', 's' };
static const symbol s_1_30[3] = { 'e', 'n', 's' };
static const symbol s_1_31[3] = { 'l', 'o', 's' };
static const symbol s_1_32[5] = { 's', 'e', 'l', 'o', 's' };
static const symbol s_1_33[3] = { 'n', 'o', 's' };
static const symbol s_1_34[4] = { '-', 'n', 'o', 's' };
static const symbol s_1_35[3] = { 'v', 'o', 's' };
static const symbol s_1_36[2] = { 'u', 's' };
static const symbol s_1_37[3] = { '-', 'u', 's' };
static const symbol s_1_38[2] = { '\'', 't' };

static const struct among a_1[39] =
{
/*  0 */ { 2, s_1_0, -1, 1, 0},
/*  1 */ { 3, s_1_1, 0, 1, 0},
/*  2 */ { 4, s_1_2, 0, 1, 0},
/*  3 */ { 2, s_1_3, -1, 1, 0},
/*  4 */ { 2, s_1_4, -1, 1, 0},
/*  5 */ { 3, s_1_5, 4, 1, 0},
/*  6 */ { 2, s_1_6, -1, 1, 0},
/*  7 */ { 3, s_1_7, -1, 1, 0},
/*  8 */ { 2, s_1_8, -1, 1, 0},
/*  9 */ { 3, s_1_9, 8, 1, 0},
/* 10 */ { 2, s_1_10, -1, 1, 0},
/* 11 */ { 3, s_1_11, 10, 1, 0},
/* 12 */ { 2, s_1_12, -1, 1, 0},
/* 13 */ { 2, s_1_13, -1, 1, 0},
/* 14 */ { 2, s_1_14, -1, 1, 0},
/* 15 */ { 2, s_1_15, -1, 1, 0},
/* 16 */ { 2, s_1_16, -1, 1, 0},
/* 17 */ { 2, s_1_17, -1, 1, 0},
/* 18 */ { 3, s_1_18, 17, 1, 0},
/* 19 */ { 2, s_1_19, -1, 1, 0},
/* 20 */ { 4, s_1_20, 19, 1, 0},
/* 21 */ { 2, s_1_21, -1, 1, 0},
/* 22 */ { 3, s_1_22, -1, 1, 0},
/* 23 */ { 5, s_1_23, 22, 1, 0},
/* 24 */ { 3, s_1_24, -1, 1, 0},
/* 25 */ { 4, s_1_25, 24, 1, 0},
/* 26 */ { 3, s_1_26, -1, 1, 0},
/* 27 */ { 3, s_1_27, -1, 1, 0},
/* 28 */ { 3, s_1_28, -1, 1, 0},
/* 29 */ { 3, s_1_29, -1, 1, 0},
/* 30 */ { 3, s_1_30, -1, 1, 0},
/* 31 */ { 3, s_1_31, -1, 1, 0},
/* 32 */ { 5, s_1_32, 31, 1, 0},
/* 33 */ { 3, s_1_33, -1, 1, 0},
/* 34 */ { 4, s_1_34, 33, 1, 0},
/* 35 */ { 3, s_1_35, -1, 1, 0},
/* 36 */ { 2, s_1_36, -1, 1, 0},
/* 37 */ { 3, s_1_37, 36, 1, 0},
/* 38 */ { 2, s_1_38, -1, 1, 0}
};

static const symbol s_2_0[3] = { 'i', 'c', 'a' };
static const symbol s_2_1[6] = { 'l', 0xF3, 'g', 'i', 'c', 'a' };
static const symbol s_2_2[4] = { 'e', 'n', 'c', 'a' };
static const symbol s_2_3[3] = { 'a', 'd', 'a' };
static const symbol s_2_4[5] = { 'a', 'n', 'c', 'i', 'a' };
static const symbol s_2_5[5] = { 'e', 'n', 'c', 'i', 'a' };
static const symbol s_2_6[5] = { 0xE8, 'n', 'c', 'i', 'a' };
static const symbol s_2_7[4] = { 0xED, 'c', 'i', 'a' };
static const symbol s_2_8[5] = { 'l', 'o', 'g', 'i', 'a' };
static const symbol s_2_9[4] = { 'i', 'n', 'i', 'a' };
static const symbol s_2_10[5] = { 0xED, 'i', 'n', 'i', 'a' };
static const symbol s_2_11[4] = { 'e', 'r', 'i', 'a' };
static const symbol s_2_12[4] = { 0xE0, 'r', 'i', 'a' };
static const symbol s_2_13[6] = { 'a', 't', 0xF2, 'r', 'i', 'a' };
static const symbol s_2_14[4] = { 'a', 'l', 'l', 'a' };
static const symbol s_2_15[4] = { 'e', 'l', 'l', 'a' };
static const symbol s_2_16[5] = { 0xED, 'v', 'o', 'l', 'a' };
static const symbol s_2_17[3] = { 'i', 'm', 'a' };
static const symbol s_2_18[6] = { 0xED, 's', 's', 'i', 'm', 'a' };
static const symbol s_2_19[8] = { 'q', 'u', 0xED, 's', 's', 'i', 'm', 'a' };
static const symbol s_2_20[3] = { 'a', 'n', 'a' };
static const symbol s_2_21[3] = { 'i', 'n', 'a' };
static const symbol s_2_22[3] = { 'e', 'r', 'a' };
static const symbol s_2_23[5] = { 's', 'f', 'e', 'r', 'a' };
static const symbol s_2_24[3] = { 'o', 'r', 'a' };
static const symbol s_2_25[4] = { 'd', 'o', 'r', 'a' };
static const symbol s_2_26[5] = { 'a', 'd', 'o', 'r', 'a' };
static const symbol s_2_27[5] = { 'a', 'd', 'u', 'r', 'a' };
static const symbol s_2_28[3] = { 'e', 's', 'a' };
static const symbol s_2_29[3] = { 'o', 's', 'a' };
static const symbol s_2_30[4] = { 'a', 's', 's', 'a' };
static const symbol s_2_31[4] = { 'e', 's', 's', 'a' };
static const symbol s_2_32[4] = { 'i', 's', 's', 'a' };
static const symbol s_2_33[3] = { 'e', 't', 'a' };
static const symbol s_2_34[3] = { 'i', 't', 'a' };
static const symbol s_2_35[3] = { 'o', 't', 'a' };
static const symbol s_2_36[4] = { 'i', 's', 't', 'a' };
static const symbol s_2_37[7] = { 'i', 'a', 'l', 'i', 's', 't', 'a' };
static const symbol s_2_38[7] = { 'i', 'o', 'n', 'i', 's', 't', 'a' };
static const symbol s_2_39[3] = { 'i', 'v', 'a' };
static const symbol s_2_40[5] = { 'a', 't', 'i', 'v', 'a' };
static const symbol s_2_41[3] = { 'n', 0xE7, 'a' };
static const symbol s_2_42[5] = { 'l', 'o', 'g', 0xED, 'a' };
static const symbol s_2_43[2] = { 'i', 'c' };
static const symbol s_2_44[5] = { 0xED, 's', 't', 'i', 'c' };
static const symbol s_2_45[3] = { 'e', 'n', 'c' };
static const symbol s_2_46[3] = { 'e', 's', 'c' };
static const symbol s_2_47[2] = { 'u', 'd' };
static const symbol s_2_48[4] = { 'a', 't', 'g', 'e' };
static const symbol s_2_49[3] = { 'b', 'l', 'e' };
static const symbol s_2_50[4] = { 'a', 'b', 'l', 'e' };
static const symbol s_2_51[4] = { 'i', 'b', 'l', 'e' };
static const symbol s_2_52[4] = { 'i', 's', 'm', 'e' };
static const symbol s_2_53[7] = { 'i', 'a', 'l', 'i', 's', 'm', 'e' };
static const symbol s_2_54[7] = { 'i', 'o', 'n', 'i', 's', 'm', 'e' };
static const symbol s_2_55[6] = { 'i', 'v', 'i', 's', 'm', 'e' };
static const symbol s_2_56[4] = { 'a', 'i', 'r', 'e' };
static const symbol s_2_57[4] = { 'i', 'c', 't', 'e' };
static const symbol s_2_58[4] = { 'i', 's', 't', 'e' };
static const symbol s_2_59[3] = { 'i', 'c', 'i' };
static const symbol s_2_60[3] = { 0xED, 'c', 'i' };
static const symbol s_2_61[4] = { 'l', 'o', 'g', 'i' };
static const symbol s_2_62[3] = { 'a', 'r', 'i' };
static const symbol s_2_63[4] = { 't', 'o', 'r', 'i' };
static const symbol s_2_64[2] = { 'a', 'l' };
static const symbol s_2_65[2] = { 'i', 'l' };
static const symbol s_2_66[3] = { 'a', 'l', 'l' };
static const symbol s_2_67[3] = { 'e', 'l', 'l' };
static const symbol s_2_68[4] = { 0xED, 'v', 'o', 'l' };
static const symbol s_2_69[4] = { 'i', 's', 'a', 'm' };
static const symbol s_2_70[5] = { 'i', 's', 's', 'e', 'm' };
static const symbol s_2_71[5] = { 0xEC, 's', 's', 'e', 'm' };
static const symbol s_2_72[5] = { 0xED, 's', 's', 'e', 'm' };
static const symbol s_2_73[5] = { 0xED, 's', 's', 'i', 'm' };
static const symbol s_2_74[7] = { 'q', 'u', 0xED, 's', 's', 'i', 'm' };
static const symbol s_2_75[4] = { 'a', 'm', 'e', 'n' };
static const symbol s_2_76[5] = { 0xEC, 's', 's', 'i', 'n' };
static const symbol s_2_77[2] = { 'a', 'r' };
static const symbol s_2_78[6] = { 'i', 'f', 'i', 'c', 'a', 'r' };
static const symbol s_2_79[4] = { 'e', 'g', 'a', 'r' };
static const symbol s_2_80[4] = { 'e', 'j', 'a', 'r' };
static const symbol s_2_81[4] = { 'i', 't', 'a', 'r' };
static const symbol s_2_82[5] = { 'i', 't', 'z', 'a', 'r' };
static const symbol s_2_83[3] = { 'f', 'e', 'r' };
static const symbol s_2_84[2] = { 'o', 'r' };
static const symbol s_2_85[3] = { 'd', 'o', 'r' };
static const symbol s_2_86[3] = { 'd', 'u', 'r' };
static const symbol s_2_87[5] = { 'd', 'o', 'r', 'a', 's' };
static const symbol s_2_88[3] = { 'i', 'c', 's' };
static const symbol s_2_89[6] = { 'l', 0xF3, 'g', 'i', 'c', 's' };
static const symbol s_2_90[3] = { 'u', 'd', 's' };
static const symbol s_2_91[4] = { 'n', 'c', 'e', 's' };
static const symbol s_2_92[4] = { 'a', 'd', 'e', 's' };
static const symbol s_2_93[6] = { 'a', 'n', 'c', 'i', 'e', 's' };
static const symbol s_2_94[6] = { 'e', 'n', 'c', 'i', 'e', 's' };
static const symbol s_2_95[6] = { 0xE8, 'n', 'c', 'i', 'e', 's' };
static const symbol s_2_96[5] = { 0xED, 'c', 'i', 'e', 's' };
static const symbol s_2_97[6] = { 'l', 'o', 'g', 'i', 'e', 's' };
static const symbol s_2_98[5] = { 'i', 'n', 'i', 'e', 's' };
static const symbol s_2_99[5] = { 0xED, 'n', 'i', 'e', 's' };
static const symbol s_2_100[5] = { 'e', 'r', 'i', 'e', 's' };
static const symbol s_2_101[5] = { 0xE0, 'r', 'i', 'e', 's' };
static const symbol s_2_102[7] = { 'a', 't', 0xF2, 'r', 'i', 'e', 's' };
static const symbol s_2_103[4] = { 'b', 'l', 'e', 's' };
static const symbol s_2_104[5] = { 'a', 'b', 'l', 'e', 's' };
static const symbol s_2_105[5] = { 'i', 'b', 'l', 'e', 's' };
static const symbol s_2_106[4] = { 'i', 'm', 'e', 's' };
static const symbol s_2_107[7] = { 0xED, 's', 's', 'i', 'm', 'e', 's' };
static const symbol s_2_108[9] = { 'q', 'u', 0xED, 's', 's', 'i', 'm', 'e', 's' };
static const symbol s_2_109[6] = { 'f', 'o', 'r', 'm', 'e', 's' };
static const symbol s_2_110[5] = { 'i', 's', 'm', 'e', 's' };
static const symbol s_2_111[8] = { 'i', 'a', 'l', 'i', 's', 'm', 'e', 's' };
static const symbol s_2_112[4] = { 'i', 'n', 'e', 's' };
static const symbol s_2_113[4] = { 'e', 'r', 'e', 's' };
static const symbol s_2_114[4] = { 'o', 'r', 'e', 's' };
static const symbol s_2_115[5] = { 'd', 'o', 'r', 'e', 's' };
static const symbol s_2_116[6] = { 'i', 'd', 'o', 'r', 'e', 's' };
static const symbol s_2_117[5] = { 'd', 'u', 'r', 'e', 's' };
static const symbol s_2_118[4] = { 'e', 's', 'e', 's' };
static const symbol s_2_119[4] = { 'o', 's', 'e', 's' };
static const symbol s_2_120[5] = { 'a', 's', 's', 'e', 's' };
static const symbol s_2_121[5] = { 'i', 'c', 't', 'e', 's' };
static const symbol s_2_122[4] = { 'i', 't', 'e', 's' };
static const symbol s_2_123[4] = { 'o', 't', 'e', 's' };
static const symbol s_2_124[5] = { 'i', 's', 't', 'e', 's' };
static const symbol s_2_125[8] = { 'i', 'a', 'l', 'i', 's', 't', 'e', 's' };
static const symbol s_2_126[8] = { 'i', 'o', 'n', 'i', 's', 't', 'e', 's' };
static const symbol s_2_127[5] = { 'i', 'q', 'u', 'e', 's' };
static const symbol s_2_128[8] = { 'l', 0xF3, 'g', 'i', 'q', 'u', 'e', 's' };
static const symbol s_2_129[4] = { 'i', 'v', 'e', 's' };
static const symbol s_2_130[6] = { 'a', 't', 'i', 'v', 'e', 's' };
static const symbol s_2_131[6] = { 'l', 'o', 'g', 0xED, 'e', 's' };
static const symbol s_2_132[9] = { 'a', 'l', 'l', 'e', 'n', 'g', 0xFC, 'e', 's' };
static const symbol s_2_133[4] = { 'i', 'c', 'i', 's' };
static const symbol s_2_134[4] = { 0xED, 'c', 'i', 's' };
static const symbol s_2_135[5] = { 'l', 'o', 'g', 'i', 's' };
static const symbol s_2_136[4] = { 'a', 'r', 'i', 's' };
static const symbol s_2_137[5] = { 't', 'o', 'r', 'i', 's' };
static const symbol s_2_138[2] = { 'l', 's' };
static const symbol s_2_139[3] = { 'a', 'l', 's' };
static const symbol s_2_140[4] = { 'e', 'l', 'l', 's' };
static const symbol s_2_141[3] = { 'i', 'm', 's' };
static const symbol s_2_142[6] = { 0xED, 's', 's', 'i', 'm', 's' };
static const symbol s_2_143[8] = { 'q', 'u', 0xED, 's', 's', 'i', 'm', 's' };
static const symbol s_2_144[4] = { 'i', 'o', 'n', 's' };
static const symbol s_2_145[5] = { 'c', 'i', 'o', 'n', 's' };
static const symbol s_2_146[6] = { 'a', 'c', 'i', 'o', 'n', 's' };
static const symbol s_2_147[4] = { 'e', 's', 'o', 's' };
static const symbol s_2_148[4] = { 'o', 's', 'o', 's' };
static const symbol s_2_149[5] = { 'a', 's', 's', 'o', 's' };
static const symbol s_2_150[5] = { 'i', 's', 's', 'o', 's' };
static const symbol s_2_151[3] = { 'e', 'r', 's' };
static const symbol s_2_152[3] = { 'o', 'r', 's' };
static const symbol s_2_153[4] = { 'd', 'o', 'r', 's' };
static const symbol s_2_154[5] = { 'a', 'd', 'o', 'r', 's' };
static const symbol s_2_155[5] = { 'i', 'd', 'o', 'r', 's' };
static const symbol s_2_156[3] = { 'a', 't', 's' };
static const symbol s_2_157[5] = { 'i', 't', 'a', 't', 's' };
static const symbol s_2_158[8] = { 'b', 'i', 'l', 'i', 't', 'a', 't', 's' };
static const symbol s_2_159[7] = { 'i', 'v', 'i', 't', 'a', 't', 's' };
static const symbol s_2_160[9] = { 'a', 't', 'i', 'v', 'i', 't', 'a', 't', 's' };
static const symbol s_2_161[5] = { 0xEF, 't', 'a', 't', 's' };
static const symbol s_2_162[3] = { 'e', 't', 's' };
static const symbol s_2_163[4] = { 'a', 'n', 't', 's' };
static const symbol s_2_164[4] = { 'e', 'n', 't', 's' };
static const symbol s_2_165[5] = { 'm', 'e', 'n', 't', 's' };
static const symbol s_2_166[6] = { 'a', 'm', 'e', 'n', 't', 's' };
static const symbol s_2_167[3] = { 'o', 't', 's' };
static const symbol s_2_168[3] = { 'u', 't', 's' };
static const symbol s_2_169[3] = { 'i', 'u', 's' };
static const symbol s_2_170[5] = { 't', 'r', 'i', 'u', 's' };
static const symbol s_2_171[5] = { 'a', 't', 'i', 'u', 's' };
static const symbol s_2_172[2] = { 0xE8, 's' };
static const symbol s_2_173[2] = { 0xE9, 's' };
static const symbol s_2_174[2] = { 0xED, 's' };
static const symbol s_2_175[3] = { 'd', 0xED, 's' };
static const symbol s_2_176[2] = { 0xF3, 's' };
static const symbol s_2_177[4] = { 'i', 't', 'a', 't' };
static const symbol s_2_178[7] = { 'b', 'i', 'l', 'i', 't', 'a', 't' };
static const symbol s_2_179[6] = { 'i', 'v', 'i', 't', 'a', 't' };
static const symbol s_2_180[8] = { 'a', 't', 'i', 'v', 'i', 't', 'a', 't' };
static const symbol s_2_181[4] = { 0xEF, 't', 'a', 't' };
static const symbol s_2_182[2] = { 'e', 't' };
static const symbol s_2_183[3] = { 'a', 'n', 't' };
static const symbol s_2_184[3] = { 'e', 'n', 't' };
static const symbol s_2_185[4] = { 'i', 'e', 'n', 't' };
static const symbol s_2_186[4] = { 'm', 'e', 'n', 't' };
static const symbol s_2_187[5] = { 'a', 'm', 'e', 'n', 't' };
static const symbol s_2_188[7] = { 'i', 's', 'a', 'm', 'e', 'n', 't' };
static const symbol s_2_189[2] = { 'o', 't' };
static const symbol s_2_190[5] = { 'i', 's', 's', 'e', 'u' };
static const symbol s_2_191[5] = { 0xEC, 's', 's', 'e', 'u' };
static const symbol s_2_192[5] = { 0xED, 's', 's', 'e', 'u' };
static const symbol s_2_193[4] = { 't', 'r', 'i', 'u' };
static const symbol s_2_194[5] = { 0xED, 's', 's', 'i', 'u' };
static const symbol s_2_195[4] = { 'a', 't', 'i', 'u' };
static const symbol s_2_196[1] = { 0xF3 };
static const symbol s_2_197[2] = { 'i', 0xF3 };
static const symbol s_2_198[3] = { 'c', 'i', 0xF3 };
static const symbol s_2_199[4] = { 'a', 'c', 'i', 0xF3 };

static const struct among a_2[200] =
{
/*  0 */ { 3, s_2_0, -1, 4, 0},
/*  1 */ { 6, s_2_1, 0, 3, 0},
/*  2 */ { 4, s_2_2, -1, 1, 0},
/*  3 */ { 3, s_2_3, -1, 2, 0},
/*  4 */ { 5, s_2_4, -1, 1, 0},
/*  5 */ { 5, s_2_5, -1, 1, 0},
/*  6 */ { 5, s_2_6, -1, 1, 0},
/*  7 */ { 4, s_2_7, -1, 1, 0},
/*  8 */ { 5, s_2_8, -1, 3, 0},
/*  9 */ { 4, s_2_9, -1, 1, 0},
/* 10 */ { 5, s_2_10, 9, 1, 0},
/* 11 */ { 4, s_2_11, -1, 1, 0},
/* 12 */ { 4, s_2_12, -1, 1, 0},
/* 13 */ { 6, s_2_13, -1, 1, 0},
/* 14 */ { 4, s_2_14, -1, 1, 0},
/* 15 */ { 4, s_2_15, -1, 1, 0},
/* 16 */ { 5, s_2_16, -1, 1, 0},
/* 17 */ { 3, s_2_17, -1, 1, 0},
/* 18 */ { 6, s_2_18, 17, 1, 0},
/* 19 */ { 8, s_2_19, 18, 5, 0},
/* 20 */ { 3, s_2_20, -1, 1, 0},
/* 21 */ { 3, s_2_21, -1, 1, 0},
/* 22 */ { 3, s_2_22, -1, 1, 0},
/* 23 */ { 5, s_2_23, 22, 1, 0},
/* 24 */ { 3, s_2_24, -1, 1, 0},
/* 25 */ { 4, s_2_25, 24, 1, 0},
/* 26 */ { 5, s_2_26, 25, 1, 0},
/* 27 */ { 5, s_2_27, -1, 1, 0},
/* 28 */ { 3, s_2_28, -1, 1, 0},
/* 29 */ { 3, s_2_29, -1, 1, 0},
/* 30 */ { 4, s_2_30, -1, 1, 0},
/* 31 */ { 4, s_2_31, -1, 1, 0},
/* 32 */ { 4, s_2_32, -1, 1, 0},
/* 33 */ { 3, s_2_33, -1, 1, 0},
/* 34 */ { 3, s_2_34, -1, 1, 0},
/* 35 */ { 3, s_2_35, -1, 1, 0},
/* 36 */ { 4, s_2_36, -1, 1, 0},
/* 37 */ { 7, s_2_37, 36, 1, 0},
/* 38 */ { 7, s_2_38, 36, 1, 0},
/* 39 */ { 3, s_2_39, -1, 1, 0},
/* 40 */ { 5, s_2_40, 39, 1, 0},
/* 41 */ { 3, s_2_41, -1, 1, 0},
/* 42 */ { 5, s_2_42, -1, 3, 0},
/* 43 */ { 2, s_2_43, -1, 4, 0},
/* 44 */ { 5, s_2_44, 43, 1, 0},
/* 45 */ { 3, s_2_45, -1, 1, 0},
/* 46 */ { 3, s_2_46, -1, 1, 0},
/* 47 */ { 2, s_2_47, -1, 1, 0},
/* 48 */ { 4, s_2_48, -1, 1, 0},
/* 49 */ { 3, s_2_49, -1, 1, 0},
/* 50 */ { 4, s_2_50, 49, 1, 0},
/* 51 */ { 4, s_2_51, 49, 1, 0},
/* 52 */ { 4, s_2_52, -1, 1, 0},
/* 53 */ { 7, s_2_53, 52, 1, 0},
/* 54 */ { 7, s_2_54, 52, 1, 0},
/* 55 */ { 6, s_2_55, 52, 1, 0},
/* 56 */ { 4, s_2_56, -1, 1, 0},
/* 57 */ { 4, s_2_57, -1, 1, 0},
/* 58 */ { 4, s_2_58, -1, 1, 0},
/* 59 */ { 3, s_2_59, -1, 1, 0},
/* 60 */ { 3, s_2_60, -1, 1, 0},
/* 61 */ { 4, s_2_61, -1, 3, 0},
/* 62 */ { 3, s_2_62, -1, 1, 0},
/* 63 */ { 4, s_2_63, -1, 1, 0},
/* 64 */ { 2, s_2_64, -1, 1, 0},
/* 65 */ { 2, s_2_65, -1, 1, 0},
/* 66 */ { 3, s_2_66, -1, 1, 0},
/* 67 */ { 3, s_2_67, -1, 1, 0},
/* 68 */ { 4, s_2_68, -1, 1, 0},
/* 69 */ { 4, s_2_69, -1, 1, 0},
/* 70 */ { 5, s_2_70, -1, 1, 0},
/* 71 */ { 5, s_2_71, -1, 1, 0},
/* 72 */ { 5, s_2_72, -1, 1, 0},
/* 73 */ { 5, s_2_73, -1, 1, 0},
/* 74 */ { 7, s_2_74, 73, 5, 0},
/* 75 */ { 4, s_2_75, -1, 1, 0},
/* 76 */ { 5, s_2_76, -1, 1, 0},
/* 77 */ { 2, s_2_77, -1, 1, 0},
/* 78 */ { 6, s_2_78, 77, 1, 0},
/* 79 */ { 4, s_2_79, 77, 1, 0},
/* 80 */ { 4, s_2_80, 77, 1, 0},
/* 81 */ { 4, s_2_81, 77, 1, 0},
/* 82 */ { 5, s_2_82, 77, 1, 0},
/* 83 */ { 3, s_2_83, -1, 1, 0},
/* 84 */ { 2, s_2_84, -1, 1, 0},
/* 85 */ { 3, s_2_85, 84, 1, 0},
/* 86 */ { 3, s_2_86, -1, 1, 0},
/* 87 */ { 5, s_2_87, -1, 1, 0},
/* 88 */ { 3, s_2_88, -1, 4, 0},
/* 89 */ { 6, s_2_89, 88, 3, 0},
/* 90 */ { 3, s_2_90, -1, 1, 0},
/* 91 */ { 4, s_2_91, -1, 1, 0},
/* 92 */ { 4, s_2_92, -1, 2, 0},
/* 93 */ { 6, s_2_93, -1, 1, 0},
/* 94 */ { 6, s_2_94, -1, 1, 0},
/* 95 */ { 6, s_2_95, -1, 1, 0},
/* 96 */ { 5, s_2_96, -1, 1, 0},
/* 97 */ { 6, s_2_97, -1, 3, 0},
/* 98 */ { 5, s_2_98, -1, 1, 0},
/* 99 */ { 5, s_2_99, -1, 1, 0},
/*100 */ { 5, s_2_100, -1, 1, 0},
/*101 */ { 5, s_2_101, -1, 1, 0},
/*102 */ { 7, s_2_102, -1, 1, 0},
/*103 */ { 4, s_2_103, -1, 1, 0},
/*104 */ { 5, s_2_104, 103, 1, 0},
/*105 */ { 5, s_2_105, 103, 1, 0},
/*106 */ { 4, s_2_106, -1, 1, 0},
/*107 */ { 7, s_2_107, 106, 1, 0},
/*108 */ { 9, s_2_108, 107, 5, 0},
/*109 */ { 6, s_2_109, -1, 1, 0},
/*110 */ { 5, s_2_110, -1, 1, 0},
/*111 */ { 8, s_2_111, 110, 1, 0},
/*112 */ { 4, s_2_112, -1, 1, 0},
/*113 */ { 4, s_2_113, -1, 1, 0},
/*114 */ { 4, s_2_114, -1, 1, 0},
/*115 */ { 5, s_2_115, 114, 1, 0},
/*116 */ { 6, s_2_116, 115, 1, 0},
/*117 */ { 5, s_2_117, -1, 1, 0},
/*118 */ { 4, s_2_118, -1, 1, 0},
/*119 */ { 4, s_2_119, -1, 1, 0},
/*120 */ { 5, s_2_120, -1, 1, 0},
/*121 */ { 5, s_2_121, -1, 1, 0},
/*122 */ { 4, s_2_122, -1, 1, 0},
/*123 */ { 4, s_2_123, -1, 1, 0},
/*124 */ { 5, s_2_124, -1, 1, 0},
/*125 */ { 8, s_2_125, 124, 1, 0},
/*126 */ { 8, s_2_126, 124, 1, 0},
/*127 */ { 5, s_2_127, -1, 4, 0},
/*128 */ { 8, s_2_128, 127, 3, 0},
/*129 */ { 4, s_2_129, -1, 1, 0},
/*130 */ { 6, s_2_130, 129, 1, 0},
/*131 */ { 6, s_2_131, -1, 3, 0},
/*132 */ { 9, s_2_132, -1, 1, 0},
/*133 */ { 4, s_2_133, -1, 1, 0},
/*134 */ { 4, s_2_134, -1, 1, 0},
/*135 */ { 5, s_2_135, -1, 3, 0},
/*136 */ { 4, s_2_136, -1, 1, 0},
/*137 */ { 5, s_2_137, -1, 1, 0},
/*138 */ { 2, s_2_138, -1, 1, 0},
/*139 */ { 3, s_2_139, 138, 1, 0},
/*140 */ { 4, s_2_140, 138, 1, 0},
/*141 */ { 3, s_2_141, -1, 1, 0},
/*142 */ { 6, s_2_142, 141, 1, 0},
/*143 */ { 8, s_2_143, 142, 5, 0},
/*144 */ { 4, s_2_144, -1, 1, 0},
/*145 */ { 5, s_2_145, 144, 1, 0},
/*146 */ { 6, s_2_146, 145, 2, 0},
/*147 */ { 4, s_2_147, -1, 1, 0},
/*148 */ { 4, s_2_148, -1, 1, 0},
/*149 */ { 5, s_2_149, -1, 1, 0},
/*150 */ { 5, s_2_150, -1, 1, 0},
/*151 */ { 3, s_2_151, -1, 1, 0},
/*152 */ { 3, s_2_152, -1, 1, 0},
/*153 */ { 4, s_2_153, 152, 1, 0},
/*154 */ { 5, s_2_154, 153, 1, 0},
/*155 */ { 5, s_2_155, 153, 1, 0},
/*156 */ { 3, s_2_156, -1, 1, 0},
/*157 */ { 5, s_2_157, 156, 1, 0},
/*158 */ { 8, s_2_158, 157, 1, 0},
/*159 */ { 7, s_2_159, 157, 1, 0},
/*160 */ { 9, s_2_160, 159, 1, 0},
/*161 */ { 5, s_2_161, 156, 1, 0},
/*162 */ { 3, s_2_162, -1, 1, 0},
/*163 */ { 4, s_2_163, -1, 1, 0},
/*164 */ { 4, s_2_164, -1, 1, 0},
/*165 */ { 5, s_2_165, 164, 1, 0},
/*166 */ { 6, s_2_166, 165, 1, 0},
/*167 */ { 3, s_2_167, -1, 1, 0},
/*168 */ { 3, s_2_168, -1, 1, 0},
/*169 */ { 3, s_2_169, -1, 1, 0},
/*170 */ { 5, s_2_170, 169, 1, 0},
/*171 */ { 5, s_2_171, 169, 1, 0},
/*172 */ { 2, s_2_172, -1, 1, 0},
/*173 */ { 2, s_2_173, -1, 1, 0},
/*174 */ { 2, s_2_174, -1, 1, 0},
/*175 */ { 3, s_2_175, 174, 1, 0},
/*176 */ { 2, s_2_176, -1, 1, 0},
/*177 */ { 4, s_2_177, -1, 1, 0},
/*178 */ { 7, s_2_178, 177, 1, 0},
/*179 */ { 6, s_2_179, 177, 1, 0},
/*180 */ { 8, s_2_180, 179, 1, 0},
/*181 */ { 4, s_2_181, -1, 1, 0},
/*182 */ { 2, s_2_182, -1, 1, 0},
/*183 */ { 3, s_2_183, -1, 1, 0},
/*184 */ { 3, s_2_184, -1, 1, 0},
/*185 */ { 4, s_2_185, 184, 1, 0},
/*186 */ { 4, s_2_186, 184, 1, 0},
/*187 */ { 5, s_2_187, 186, 1, 0},
/*188 */ { 7, s_2_188, 187, 1, 0},
/*189 */ { 2, s_2_189, -1, 1, 0},
/*190 */ { 5, s_2_190, -1, 1, 0},
/*191 */ { 5, s_2_191, -1, 1, 0},
/*192 */ { 5, s_2_192, -1, 1, 0},
/*193 */ { 4, s_2_193, -1, 1, 0},
/*194 */ { 5, s_2_194, -1, 1, 0},
/*195 */ { 4, s_2_195, -1, 1, 0},
/*196 */ { 1, s_2_196, -1, 1, 0},
/*197 */ { 2, s_2_197, 196, 1, 0},
/*198 */ { 3, s_2_198, 197, 1, 0},
/*199 */ { 4, s_2_199, 198, 1, 0}
};

static const symbol s_3_0[3] = { 'a', 'b', 'a' };
static const symbol s_3_1[4] = { 'e', 's', 'c', 'a' };
static const symbol s_3_2[4] = { 'i', 's', 'c', 'a' };
static const symbol s_3_3[4] = { 0xEF, 's', 'c', 'a' };
static const symbol s_3_4[3] = { 'a', 'd', 'a' };
static const symbol s_3_5[3] = { 'i', 'd', 'a' };
static const symbol s_3_6[3] = { 'u', 'd', 'a' };
static const symbol s_3_7[3] = { 0xEF, 'd', 'a' };
static const symbol s_3_8[2] = { 'i', 'a' };
static const symbol s_3_9[4] = { 'a', 'r', 'i', 'a' };
static const symbol s_3_10[4] = { 'i', 'r', 'i', 'a' };
static const symbol s_3_11[3] = { 'a', 'r', 'a' };
static const symbol s_3_12[4] = { 'i', 'e', 'r', 'a' };
static const symbol s_3_13[3] = { 'i', 'r', 'a' };
static const symbol s_3_14[5] = { 'a', 'd', 'o', 'r', 'a' };
static const symbol s_3_15[3] = { 0xEF, 'r', 'a' };
static const symbol s_3_16[3] = { 'a', 'v', 'a' };
static const symbol s_3_17[3] = { 'i', 'x', 'a' };
static const symbol s_3_18[4] = { 'i', 't', 'z', 'a' };
static const symbol s_3_19[2] = { 0xED, 'a' };
static const symbol s_3_20[4] = { 'a', 'r', 0xED, 'a' };
static const symbol s_3_21[4] = { 'e', 'r', 0xED, 'a' };
static const symbol s_3_22[4] = { 'i', 'r', 0xED, 'a' };
static const symbol s_3_23[2] = { 0xEF, 'a' };
static const symbol s_3_24[3] = { 'i', 's', 'c' };
static const symbol s_3_25[3] = { 0xEF, 's', 'c' };
static const symbol s_3_26[2] = { 'a', 'd' };
static const symbol s_3_27[2] = { 'e', 'd' };
static const symbol s_3_28[2] = { 'i', 'd' };
static const symbol s_3_29[2] = { 'i', 'e' };
static const symbol s_3_30[2] = { 'r', 'e' };
static const symbol s_3_31[3] = { 'd', 'r', 'e' };
static const symbol s_3_32[3] = { 'a', 's', 'e' };
static const symbol s_3_33[4] = { 'i', 'e', 's', 'e' };
static const symbol s_3_34[4] = { 'a', 's', 't', 'e' };
static const symbol s_3_35[4] = { 'i', 's', 't', 'e' };
static const symbol s_3_36[2] = { 'i', 'i' };
static const symbol s_3_37[3] = { 'i', 'n', 'i' };
static const symbol s_3_38[5] = { 'e', 's', 'q', 'u', 'i' };
static const symbol s_3_39[4] = { 'e', 'i', 'x', 'i' };
static const symbol s_3_40[4] = { 'i', 't', 'z', 'i' };
static const symbol s_3_41[2] = { 'a', 'm' };
static const symbol s_3_42[2] = { 'e', 'm' };
static const symbol s_3_43[4] = { 'a', 'r', 'e', 'm' };
static const symbol s_3_44[4] = { 'i', 'r', 'e', 'm' };
static const symbol s_3_45[4] = { 0xE0, 'r', 'e', 'm' };
static const symbol s_3_46[4] = { 0xED, 'r', 'e', 'm' };
static const symbol s_3_47[5] = { 0xE0, 's', 's', 'e', 'm' };
static const symbol s_3_48[5] = { 0xE9, 's', 's', 'e', 'm' };
static const symbol s_3_49[5] = { 'i', 'g', 'u', 'e', 'm' };
static const symbol s_3_50[5] = { 0xEF, 'g', 'u', 'e', 'm' };
static const symbol s_3_51[4] = { 'a', 'v', 'e', 'm' };
static const symbol s_3_52[4] = { 0xE0, 'v', 'e', 'm' };
static const symbol s_3_53[4] = { 0xE1, 'v', 'e', 'm' };
static const symbol s_3_54[5] = { 'i', 'r', 0xEC, 'e', 'm' };
static const symbol s_3_55[3] = { 0xED, 'e', 'm' };
static const symbol s_3_56[5] = { 'a', 'r', 0xED, 'e', 'm' };
static const symbol s_3_57[5] = { 'i', 'r', 0xED, 'e', 'm' };
static const symbol s_3_58[5] = { 'a', 's', 's', 'i', 'm' };
static const symbol s_3_59[5] = { 'e', 's', 's', 'i', 'm' };
static const symbol s_3_60[5] = { 'i', 's', 's', 'i', 'm' };
static const symbol s_3_61[5] = { 0xE0, 's', 's', 'i', 'm' };
static const symbol s_3_62[5] = { 0xE8, 's', 's', 'i', 'm' };
static const symbol s_3_63[5] = { 0xE9, 's', 's', 'i', 'm' };
static const symbol s_3_64[5] = { 0xED, 's', 's', 'i', 'm' };
static const symbol s_3_65[2] = { 0xEF, 'm' };
static const symbol s_3_66[2] = { 'a', 'n' };
static const symbol s_3_67[4] = { 'a', 'b', 'a', 'n' };
static const symbol s_3_68[5] = { 'a', 'r', 'i', 'a', 'n' };
static const symbol s_3_69[4] = { 'a', 'r', 'a', 'n' };
static const symbol s_3_70[5] = { 'i', 'e', 'r', 'a', 'n' };
static const symbol s_3_71[4] = { 'i', 'r', 'a', 'n' };
static const symbol s_3_72[3] = { 0xED, 'a', 'n' };
static const symbol s_3_73[5] = { 'a', 'r', 0xED, 'a', 'n' };
static const symbol s_3_74[5] = { 'e', 'r', 0xED, 'a', 'n' };
static const symbol s_3_75[5] = { 'i', 'r', 0xED, 'a', 'n' };
static const symbol s_3_76[2] = { 'e', 'n' };
static const symbol s_3_77[3] = { 'i', 'e', 'n' };
static const symbol s_3_78[5] = { 'a', 'r', 'i', 'e', 'n' };
static const symbol s_3_79[5] = { 'i', 'r', 'i', 'e', 'n' };
static const symbol s_3_80[4] = { 'a', 'r', 'e', 'n' };
static const symbol s_3_81[4] = { 'e', 'r', 'e', 'n' };
static const symbol s_3_82[4] = { 'i', 'r', 'e', 'n' };
static const symbol s_3_83[4] = { 0xE0, 'r', 'e', 'n' };
static const symbol s_3_84[4] = { 0xEF, 'r', 'e', 'n' };
static const symbol s_3_85[4] = { 'a', 's', 'e', 'n' };
static const symbol s_3_86[5] = { 'i', 'e', 's', 'e', 'n' };
static const symbol s_3_87[5] = { 'a', 's', 's', 'e', 'n' };
static const symbol s_3_88[5] = { 'e', 's', 's', 'e', 'n' };
static const symbol s_3_89[5] = { 'i', 's', 's', 'e', 'n' };
static const symbol s_3_90[5] = { 0xE9, 's', 's', 'e', 'n' };
static const symbol s_3_91[5] = { 0xEF, 's', 's', 'e', 'n' };
static const symbol s_3_92[6] = { 'e', 's', 'q', 'u', 'e', 'n' };
static const symbol s_3_93[6] = { 'i', 's', 'q', 'u', 'e', 'n' };
static const symbol s_3_94[6] = { 0xEF, 's', 'q', 'u', 'e', 'n' };
static const symbol s_3_95[4] = { 'a', 'v', 'e', 'n' };
static const symbol s_3_96[4] = { 'i', 'x', 'e', 'n' };
static const symbol s_3_97[5] = { 'e', 'i', 'x', 'e', 'n' };
static const symbol s_3_98[4] = { 0xEF, 'x', 'e', 'n' };
static const symbol s_3_99[3] = { 0xEF, 'e', 'n' };
static const symbol s_3_100[2] = { 'i', 'n' };
static const symbol s_3_101[4] = { 'i', 'n', 'i', 'n' };
static const symbol s_3_102[3] = { 's', 'i', 'n' };
static const symbol s_3_103[4] = { 'i', 's', 'i', 'n' };
static const symbol s_3_104[5] = { 'a', 's', 's', 'i', 'n' };
static const symbol s_3_105[5] = { 'e', 's', 's', 'i', 'n' };
static const symbol s_3_106[5] = { 'i', 's', 's', 'i', 'n' };
static const symbol s_3_107[5] = { 0xEF, 's', 's', 'i', 'n' };
static const symbol s_3_108[6] = { 'e', 's', 'q', 'u', 'i', 'n' };
static const symbol s_3_109[5] = { 'e', 'i', 'x', 'i', 'n' };
static const symbol s_3_110[4] = { 'a', 'r', 'o', 'n' };
static const symbol s_3_111[5] = { 'i', 'e', 'r', 'o', 'n' };
static const symbol s_3_112[4] = { 'a', 'r', 0xE1, 'n' };
static const symbol s_3_113[4] = { 'e', 'r', 0xE1, 'n' };
static const symbol s_3_114[4] = { 'i', 'r', 0xE1, 'n' };
static const symbol s_3_115[3] = { 'i', 0xEF, 'n' };
static const symbol s_3_116[3] = { 'a', 'd', 'o' };
static const symbol s_3_117[3] = { 'i', 'd', 'o' };
static const symbol s_3_118[4] = { 'a', 'n', 'd', 'o' };
static const symbol s_3_119[5] = { 'i', 'e', 'n', 'd', 'o' };
static const symbol s_3_120[2] = { 'i', 'o' };
static const symbol s_3_121[3] = { 'i', 'x', 'o' };
static const symbol s_3_122[4] = { 'e', 'i', 'x', 'o' };
static const symbol s_3_123[3] = { 0xEF, 'x', 'o' };
static const symbol s_3_124[4] = { 'i', 't', 'z', 'o' };
static const symbol s_3_125[2] = { 'a', 'r' };
static const symbol s_3_126[4] = { 't', 'z', 'a', 'r' };
static const symbol s_3_127[2] = { 'e', 'r' };
static const symbol s_3_128[5] = { 'e', 'i', 'x', 'e', 'r' };
static const symbol s_3_129[2] = { 'i', 'r' };
static const symbol s_3_130[4] = { 'a', 'd', 'o', 'r' };
static const symbol s_3_131[2] = { 'a', 's' };
static const symbol s_3_132[4] = { 'a', 'b', 'a', 's' };
static const symbol s_3_133[4] = { 'a', 'd', 'a', 's' };
static const symbol s_3_134[4] = { 'i', 'd', 'a', 's' };
static const symbol s_3_135[4] = { 'a', 'r', 'a', 's' };
static const symbol s_3_136[5] = { 'i', 'e', 'r', 'a', 's' };
static const symbol s_3_137[3] = { 0xED, 'a', 's' };
static const symbol s_3_138[5] = { 'a', 'r', 0xED, 'a', 's' };
static const symbol s_3_139[5] = { 'e', 'r', 0xED, 'a', 's' };
static const symbol s_3_140[5] = { 'i', 'r', 0xED, 'a', 's' };
static const symbol s_3_141[3] = { 'i', 'd', 's' };
static const symbol s_3_142[2] = { 'e', 's' };
static const symbol s_3_143[4] = { 'a', 'd', 'e', 's' };
static const symbol s_3_144[4] = { 'i', 'd', 'e', 's' };
static const symbol s_3_145[4] = { 'u', 'd', 'e', 's' };
static const symbol s_3_146[4] = { 0xEF, 'd', 'e', 's' };
static const symbol s_3_147[5] = { 'a', 't', 'g', 'e', 's' };
static const symbol s_3_148[3] = { 'i', 'e', 's' };
static const symbol s_3_149[5] = { 'a', 'r', 'i', 'e', 's' };
static const symbol s_3_150[5] = { 'i', 'r', 'i', 'e', 's' };
static const symbol s_3_151[4] = { 'a', 'r', 'e', 's' };
static const symbol s_3_152[4] = { 'i', 'r', 'e', 's' };
static const symbol s_3_153[6] = { 'a', 'd', 'o', 'r', 'e', 's' };
static const symbol s_3_154[4] = { 0xEF, 'r', 'e', 's' };
static const symbol s_3_155[4] = { 'a', 's', 'e', 's' };
static const symbol s_3_156[5] = { 'i', 'e', 's', 'e', 's' };
static const symbol s_3_157[5] = { 'a', 's', 's', 'e', 's' };
static const symbol s_3_158[5] = { 'e', 's', 's', 'e', 's' };
static const symbol s_3_159[5] = { 'i', 's', 's', 'e', 's' };
static const symbol s_3_160[5] = { 0xEF, 's', 's', 'e', 's' };
static const symbol s_3_161[4] = { 'q', 'u', 'e', 's' };
static const symbol s_3_162[6] = { 'e', 's', 'q', 'u', 'e', 's' };
static const symbol s_3_163[6] = { 0xEF, 's', 'q', 'u', 'e', 's' };
static const symbol s_3_164[4] = { 'a', 'v', 'e', 's' };
static const symbol s_3_165[4] = { 'i', 'x', 'e', 's' };
static const symbol s_3_166[5] = { 'e', 'i', 'x', 'e', 's' };
static const symbol s_3_167[4] = { 0xEF, 'x', 'e', 's' };
static const symbol s_3_168[3] = { 0xEF, 'e', 's' };
static const symbol s_3_169[5] = { 'a', 'b', 'a', 'i', 's' };
static const symbol s_3_170[5] = { 'a', 'r', 'a', 'i', 's' };
static const symbol s_3_171[6] = { 'i', 'e', 'r', 'a', 'i', 's' };
static const symbol s_3_172[4] = { 0xED, 'a', 'i', 's' };
static const symbol s_3_173[6] = { 'a', 'r', 0xED, 'a', 'i', 's' };
static const symbol s_3_174[6] = { 'e', 'r', 0xED, 'a', 'i', 's' };
static const symbol s_3_175[6] = { 'i', 'r', 0xED, 'a', 'i', 's' };
static const symbol s_3_176[5] = { 'a', 's', 'e', 'i', 's' };
static const symbol s_3_177[6] = { 'i', 'e', 's', 'e', 'i', 's' };
static const symbol s_3_178[6] = { 'a', 's', 't', 'e', 'i', 's' };
static const symbol s_3_179[6] = { 'i', 's', 't', 'e', 'i', 's' };
static const symbol s_3_180[4] = { 'i', 'n', 'i', 's' };
static const symbol s_3_181[3] = { 's', 'i', 's' };
static const symbol s_3_182[4] = { 'i', 's', 'i', 's' };
static const symbol s_3_183[5] = { 'a', 's', 's', 'i', 's' };
static const symbol s_3_184[5] = { 'e', 's', 's', 'i', 's' };
static const symbol s_3_185[5] = { 'i', 's', 's', 'i', 's' };
static const symbol s_3_186[5] = { 0xEF, 's', 's', 'i', 's' };
static const symbol s_3_187[6] = { 'e', 's', 'q', 'u', 'i', 's' };
static const symbol s_3_188[5] = { 'e', 'i', 'x', 'i', 's' };
static const symbol s_3_189[5] = { 'i', 't', 'z', 'i', 's' };
static const symbol s_3_190[3] = { 0xE1, 'i', 's' };
static const symbol s_3_191[5] = { 'a', 'r', 0xE9, 'i', 's' };
static const symbol s_3_192[5] = { 'e', 'r', 0xE9, 'i', 's' };
static const symbol s_3_193[5] = { 'i', 'r', 0xE9, 'i', 's' };
static const symbol s_3_194[3] = { 'a', 'm', 's' };
static const symbol s_3_195[4] = { 'a', 'd', 'o', 's' };
static const symbol s_3_196[4] = { 'i', 'd', 'o', 's' };
static const symbol s_3_197[4] = { 'a', 'm', 'o', 's' };
static const symbol s_3_198[6] = { 0xE1, 'b', 'a', 'm', 'o', 's' };
static const symbol s_3_199[6] = { 0xE1, 'r', 'a', 'm', 'o', 's' };
static const symbol s_3_200[7] = { 'i', 0xE9, 'r', 'a', 'm', 'o', 's' };
static const symbol s_3_201[5] = { 0xED, 'a', 'm', 'o', 's' };
static const symbol s_3_202[7] = { 'a', 'r', 0xED, 'a', 'm', 'o', 's' };
static const symbol s_3_203[7] = { 'e', 'r', 0xED, 'a', 'm', 'o', 's' };
static const symbol s_3_204[7] = { 'i', 'r', 0xED, 'a', 'm', 'o', 's' };
static const symbol s_3_205[6] = { 'a', 'r', 'e', 'm', 'o', 's' };
static const symbol s_3_206[6] = { 'e', 'r', 'e', 'm', 'o', 's' };
static const symbol s_3_207[6] = { 'i', 'r', 'e', 'm', 'o', 's' };
static const symbol s_3_208[6] = { 0xE1, 's', 'e', 'm', 'o', 's' };
static const symbol s_3_209[7] = { 'i', 0xE9, 's', 'e', 'm', 'o', 's' };
static const symbol s_3_210[4] = { 'i', 'm', 'o', 's' };
static const symbol s_3_211[5] = { 'a', 'd', 'o', 'r', 's' };
static const symbol s_3_212[3] = { 'a', 's', 's' };
static const symbol s_3_213[5] = { 'e', 'r', 'a', 's', 's' };
static const symbol s_3_214[3] = { 'e', 's', 's' };
static const symbol s_3_215[3] = { 'a', 't', 's' };
static const symbol s_3_216[3] = { 'i', 't', 's' };
static const symbol s_3_217[4] = { 'e', 'n', 't', 's' };
static const symbol s_3_218[2] = { 0xE0, 's' };
static const symbol s_3_219[4] = { 'a', 'r', 0xE0, 's' };
static const symbol s_3_220[4] = { 'i', 'r', 0xE0, 's' };
static const symbol s_3_221[4] = { 'a', 'r', 0xE1, 's' };
static const symbol s_3_222[4] = { 'e', 'r', 0xE1, 's' };
static const symbol s_3_223[4] = { 'i', 'r', 0xE1, 's' };
static const symbol s_3_224[2] = { 0xE9, 's' };
static const symbol s_3_225[4] = { 'a', 'r', 0xE9, 's' };
static const symbol s_3_226[2] = { 0xED, 's' };
static const symbol s_3_227[3] = { 'i', 0xEF, 's' };
static const symbol s_3_228[2] = { 'a', 't' };
static const symbol s_3_229[2] = { 'i', 't' };
static const symbol s_3_230[3] = { 'a', 'n', 't' };
static const symbol s_3_231[3] = { 'e', 'n', 't' };
static const symbol s_3_232[3] = { 'i', 'n', 't' };
static const symbol s_3_233[2] = { 'u', 't' };
static const symbol s_3_234[2] = { 0xEF, 't' };
static const symbol s_3_235[2] = { 'a', 'u' };
static const symbol s_3_236[4] = { 'e', 'r', 'a', 'u' };
static const symbol s_3_237[3] = { 'i', 'e', 'u' };
static const symbol s_3_238[4] = { 'i', 'n', 'e', 'u' };
static const symbol s_3_239[4] = { 'a', 'r', 'e', 'u' };
static const symbol s_3_240[4] = { 'i', 'r', 'e', 'u' };
static const symbol s_3_241[4] = { 0xE0, 'r', 'e', 'u' };
static const symbol s_3_242[4] = { 0xED, 'r', 'e', 'u' };
static const symbol s_3_243[5] = { 'a', 's', 's', 'e', 'u' };
static const symbol s_3_244[5] = { 'e', 's', 's', 'e', 'u' };
static const symbol s_3_245[7] = { 'e', 'r', 'e', 's', 's', 'e', 'u' };
static const symbol s_3_246[5] = { 0xE0, 's', 's', 'e', 'u' };
static const symbol s_3_247[5] = { 0xE9, 's', 's', 'e', 'u' };
static const symbol s_3_248[5] = { 'i', 'g', 'u', 'e', 'u' };
static const symbol s_3_249[5] = { 0xEF, 'g', 'u', 'e', 'u' };
static const symbol s_3_250[4] = { 0xE0, 'v', 'e', 'u' };
static const symbol s_3_251[4] = { 0xE1, 'v', 'e', 'u' };
static const symbol s_3_252[5] = { 'i', 't', 'z', 'e', 'u' };
static const symbol s_3_253[3] = { 0xEC, 'e', 'u' };
static const symbol s_3_254[5] = { 'i', 'r', 0xEC, 'e', 'u' };
static const symbol s_3_255[3] = { 0xED, 'e', 'u' };
static const symbol s_3_256[5] = { 'a', 'r', 0xED, 'e', 'u' };
static const symbol s_3_257[5] = { 'i', 'r', 0xED, 'e', 'u' };
static const symbol s_3_258[5] = { 'a', 's', 's', 'i', 'u' };
static const symbol s_3_259[5] = { 'i', 's', 's', 'i', 'u' };
static const symbol s_3_260[5] = { 0xE0, 's', 's', 'i', 'u' };
static const symbol s_3_261[5] = { 0xE8, 's', 's', 'i', 'u' };
static const symbol s_3_262[5] = { 0xE9, 's', 's', 'i', 'u' };
static const symbol s_3_263[5] = { 0xED, 's', 's', 'i', 'u' };
static const symbol s_3_264[2] = { 0xEF, 'u' };
static const symbol s_3_265[2] = { 'i', 'x' };
static const symbol s_3_266[3] = { 'e', 'i', 'x' };
static const symbol s_3_267[2] = { 0xEF, 'x' };
static const symbol s_3_268[3] = { 'i', 't', 'z' };
static const symbol s_3_269[2] = { 'i', 0xE0 };
static const symbol s_3_270[3] = { 'a', 'r', 0xE0 };
static const symbol s_3_271[3] = { 'i', 'r', 0xE0 };
static const symbol s_3_272[4] = { 'i', 't', 'z', 0xE0 };
static const symbol s_3_273[3] = { 'a', 'r', 0xE1 };
static const symbol s_3_274[3] = { 'e', 'r', 0xE1 };
static const symbol s_3_275[3] = { 'i', 'r', 0xE1 };
static const symbol s_3_276[3] = { 'i', 'r', 0xE8 };
static const symbol s_3_277[3] = { 'a', 'r', 0xE9 };
static const symbol s_3_278[3] = { 'e', 'r', 0xE9 };
static const symbol s_3_279[3] = { 'i', 'r', 0xE9 };
static const symbol s_3_280[1] = { 0xED };
static const symbol s_3_281[2] = { 'i', 0xEF };
static const symbol s_3_282[2] = { 'i', 0xF3 };

static const struct among a_3[283] =
{
/*  0 */ { 3, s_3_0, -1, 1, 0},
/*  1 */ { 4, s_3_1, -1, 1, 0},
/*  2 */ { 4, s_3_2, -1, 1, 0},
/*  3 */ { 4, s_3_3, -1, 1, 0},
/*  4 */ { 3, s_3_4, -1, 1, 0},
/*  5 */ { 3, s_3_5, -1, 1, 0},
/*  6 */ { 3, s_3_6, -1, 1, 0},
/*  7 */ { 3, s_3_7, -1, 1, 0},
/*  8 */ { 2, s_3_8, -1, 1, 0},
/*  9 */ { 4, s_3_9, 8, 1, 0},
/* 10 */ { 4, s_3_10, 8, 1, 0},
/* 11 */ { 3, s_3_11, -1, 1, 0},
/* 12 */ { 4, s_3_12, -1, 1, 0},
/* 13 */ { 3, s_3_13, -1, 1, 0},
/* 14 */ { 5, s_3_14, -1, 1, 0},
/* 15 */ { 3, s_3_15, -1, 1, 0},
/* 16 */ { 3, s_3_16, -1, 1, 0},
/* 17 */ { 3, s_3_17, -1, 1, 0},
/* 18 */ { 4, s_3_18, -1, 1, 0},
/* 19 */ { 2, s_3_19, -1, 1, 0},
/* 20 */ { 4, s_3_20, 19, 1, 0},
/* 21 */ { 4, s_3_21, 19, 1, 0},
/* 22 */ { 4, s_3_22, 19, 1, 0},
/* 23 */ { 2, s_3_23, -1, 1, 0},
/* 24 */ { 3, s_3_24, -1, 1, 0},
/* 25 */ { 3, s_3_25, -1, 1, 0},
/* 26 */ { 2, s_3_26, -1, 1, 0},
/* 27 */ { 2, s_3_27, -1, 1, 0},
/* 28 */ { 2, s_3_28, -1, 1, 0},
/* 29 */ { 2, s_3_29, -1, 1, 0},
/* 30 */ { 2, s_3_30, -1, 1, 0},
/* 31 */ { 3, s_3_31, 30, 1, 0},
/* 32 */ { 3, s_3_32, -1, 1, 0},
/* 33 */ { 4, s_3_33, -1, 1, 0},
/* 34 */ { 4, s_3_34, -1, 1, 0},
/* 35 */ { 4, s_3_35, -1, 1, 0},
/* 36 */ { 2, s_3_36, -1, 1, 0},
/* 37 */ { 3, s_3_37, -1, 1, 0},
/* 38 */ { 5, s_3_38, -1, 1, 0},
/* 39 */ { 4, s_3_39, -1, 1, 0},
/* 40 */ { 4, s_3_40, -1, 1, 0},
/* 41 */ { 2, s_3_41, -1, 1, 0},
/* 42 */ { 2, s_3_42, -1, 1, 0},
/* 43 */ { 4, s_3_43, 42, 1, 0},
/* 44 */ { 4, s_3_44, 42, 1, 0},
/* 45 */ { 4, s_3_45, 42, 1, 0},
/* 46 */ { 4, s_3_46, 42, 1, 0},
/* 47 */ { 5, s_3_47, 42, 1, 0},
/* 48 */ { 5, s_3_48, 42, 1, 0},
/* 49 */ { 5, s_3_49, 42, 1, 0},
/* 50 */ { 5, s_3_50, 42, 1, 0},
/* 51 */ { 4, s_3_51, 42, 1, 0},
/* 52 */ { 4, s_3_52, 42, 1, 0},
/* 53 */ { 4, s_3_53, 42, 1, 0},
/* 54 */ { 5, s_3_54, 42, 1, 0},
/* 55 */ { 3, s_3_55, 42, 1, 0},
/* 56 */ { 5, s_3_56, 55, 1, 0},
/* 57 */ { 5, s_3_57, 55, 1, 0},
/* 58 */ { 5, s_3_58, -1, 1, 0},
/* 59 */ { 5, s_3_59, -1, 1, 0},
/* 60 */ { 5, s_3_60, -1, 1, 0},
/* 61 */ { 5, s_3_61, -1, 1, 0},
/* 62 */ { 5, s_3_62, -1, 1, 0},
/* 63 */ { 5, s_3_63, -1, 1, 0},
/* 64 */ { 5, s_3_64, -1, 1, 0},
/* 65 */ { 2, s_3_65, -1, 1, 0},
/* 66 */ { 2, s_3_66, -1, 1, 0},
/* 67 */ { 4, s_3_67, 66, 1, 0},
/* 68 */ { 5, s_3_68, 66, 1, 0},
/* 69 */ { 4, s_3_69, 66, 1, 0},
/* 70 */ { 5, s_3_70, 66, 1, 0},
/* 71 */ { 4, s_3_71, 66, 1, 0},
/* 72 */ { 3, s_3_72, 66, 1, 0},
/* 73 */ { 5, s_3_73, 72, 1, 0},
/* 74 */ { 5, s_3_74, 72, 1, 0},
/* 75 */ { 5, s_3_75, 72, 1, 0},
/* 76 */ { 2, s_3_76, -1, 1, 0},
/* 77 */ { 3, s_3_77, 76, 1, 0},
/* 78 */ { 5, s_3_78, 77, 1, 0},
/* 79 */ { 5, s_3_79, 77, 1, 0},
/* 80 */ { 4, s_3_80, 76, 1, 0},
/* 81 */ { 4, s_3_81, 76, 1, 0},
/* 82 */ { 4, s_3_82, 76, 1, 0},
/* 83 */ { 4, s_3_83, 76, 1, 0},
/* 84 */ { 4, s_3_84, 76, 1, 0},
/* 85 */ { 4, s_3_85, 76, 1, 0},
/* 86 */ { 5, s_3_86, 76, 1, 0},
/* 87 */ { 5, s_3_87, 76, 1, 0},
/* 88 */ { 5, s_3_88, 76, 1, 0},
/* 89 */ { 5, s_3_89, 76, 1, 0},
/* 90 */ { 5, s_3_90, 76, 1, 0},
/* 91 */ { 5, s_3_91, 76, 1, 0},
/* 92 */ { 6, s_3_92, 76, 1, 0},
/* 93 */ { 6, s_3_93, 76, 1, 0},
/* 94 */ { 6, s_3_94, 76, 1, 0},
/* 95 */ { 4, s_3_95, 76, 1, 0},
/* 96 */ { 4, s_3_96, 76, 1, 0},
/* 97 */ { 5, s_3_97, 96, 1, 0},
/* 98 */ { 4, s_3_98, 76, 1, 0},
/* 99 */ { 3, s_3_99, 76, 1, 0},
/*100 */ { 2, s_3_100, -1, 1, 0},
/*101 */ { 4, s_3_101, 100, 1, 0},
/*102 */ { 3, s_3_102, 100, 1, 0},
/*103 */ { 4, s_3_103, 102, 1, 0},
/*104 */ { 5, s_3_104, 102, 1, 0},
/*105 */ { 5, s_3_105, 102, 1, 0},
/*106 */ { 5, s_3_106, 102, 1, 0},
/*107 */ { 5, s_3_107, 102, 1, 0},
/*108 */ { 6, s_3_108, 100, 1, 0},
/*109 */ { 5, s_3_109, 100, 1, 0},
/*110 */ { 4, s_3_110, -1, 1, 0},
/*111 */ { 5, s_3_111, -1, 1, 0},
/*112 */ { 4, s_3_112, -1, 1, 0},
/*113 */ { 4, s_3_113, -1, 1, 0},
/*114 */ { 4, s_3_114, -1, 1, 0},
/*115 */ { 3, s_3_115, -1, 1, 0},
/*116 */ { 3, s_3_116, -1, 1, 0},
/*117 */ { 3, s_3_117, -1, 1, 0},
/*118 */ { 4, s_3_118, -1, 2, 0},
/*119 */ { 5, s_3_119, -1, 1, 0},
/*120 */ { 2, s_3_120, -1, 1, 0},
/*121 */ { 3, s_3_121, -1, 1, 0},
/*122 */ { 4, s_3_122, 121, 1, 0},
/*123 */ { 3, s_3_123, -1, 1, 0},
/*124 */ { 4, s_3_124, -1, 1, 0},
/*125 */ { 2, s_3_125, -1, 1, 0},
/*126 */ { 4, s_3_126, 125, 1, 0},
/*127 */ { 2, s_3_127, -1, 1, 0},
/*128 */ { 5, s_3_128, 127, 1, 0},
/*129 */ { 2, s_3_129, -1, 1, 0},
/*130 */ { 4, s_3_130, -1, 1, 0},
/*131 */ { 2, s_3_131, -1, 1, 0},
/*132 */ { 4, s_3_132, 131, 1, 0},
/*133 */ { 4, s_3_133, 131, 1, 0},
/*134 */ { 4, s_3_134, 131, 1, 0},
/*135 */ { 4, s_3_135, 131, 1, 0},
/*136 */ { 5, s_3_136, 131, 1, 0},
/*137 */ { 3, s_3_137, 131, 1, 0},
/*138 */ { 5, s_3_138, 137, 1, 0},
/*139 */ { 5, s_3_139, 137, 1, 0},
/*140 */ { 5, s_3_140, 137, 1, 0},
/*141 */ { 3, s_3_141, -1, 1, 0},
/*142 */ { 2, s_3_142, -1, 1, 0},
/*143 */ { 4, s_3_143, 142, 1, 0},
/*144 */ { 4, s_3_144, 142, 1, 0},
/*145 */ { 4, s_3_145, 142, 1, 0},
/*146 */ { 4, s_3_146, 142, 1, 0},
/*147 */ { 5, s_3_147, 142, 1, 0},
/*148 */ { 3, s_3_148, 142, 1, 0},
/*149 */ { 5, s_3_149, 148, 1, 0},
/*150 */ { 5, s_3_150, 148, 1, 0},
/*151 */ { 4, s_3_151, 142, 1, 0},
/*152 */ { 4, s_3_152, 142, 1, 0},
/*153 */ { 6, s_3_153, 142, 1, 0},
/*154 */ { 4, s_3_154, 142, 1, 0},
/*155 */ { 4, s_3_155, 142, 1, 0},
/*156 */ { 5, s_3_156, 142, 1, 0},
/*157 */ { 5, s_3_157, 142, 1, 0},
/*158 */ { 5, s_3_158, 142, 1, 0},
/*159 */ { 5, s_3_159, 142, 1, 0},
/*160 */ { 5, s_3_160, 142, 1, 0},
/*161 */ { 4, s_3_161, 142, 1, 0},
/*162 */ { 6, s_3_162, 161, 1, 0},
/*163 */ { 6, s_3_163, 161, 1, 0},
/*164 */ { 4, s_3_164, 142, 1, 0},
/*165 */ { 4, s_3_165, 142, 1, 0},
/*166 */ { 5, s_3_166, 165, 1, 0},
/*167 */ { 4, s_3_167, 142, 1, 0},
/*168 */ { 3, s_3_168, 142, 1, 0},
/*169 */ { 5, s_3_169, -1, 1, 0},
/*170 */ { 5, s_3_170, -1, 1, 0},
/*171 */ { 6, s_3_171, -1, 1, 0},
/*172 */ { 4, s_3_172, -1, 1, 0},
/*173 */ { 6, s_3_173, 172, 1, 0},
/*174 */ { 6, s_3_174, 172, 1, 0},
/*175 */ { 6, s_3_175, 172, 1, 0},
/*176 */ { 5, s_3_176, -1, 1, 0},
/*177 */ { 6, s_3_177, -1, 1, 0},
/*178 */ { 6, s_3_178, -1, 1, 0},
/*179 */ { 6, s_3_179, -1, 1, 0},
/*180 */ { 4, s_3_180, -1, 1, 0},
/*181 */ { 3, s_3_181, -1, 1, 0},
/*182 */ { 4, s_3_182, 181, 1, 0},
/*183 */ { 5, s_3_183, 181, 1, 0},
/*184 */ { 5, s_3_184, 181, 1, 0},
/*185 */ { 5, s_3_185, 181, 1, 0},
/*186 */ { 5, s_3_186, 181, 1, 0},
/*187 */ { 6, s_3_187, -1, 1, 0},
/*188 */ { 5, s_3_188, -1, 1, 0},
/*189 */ { 5, s_3_189, -1, 1, 0},
/*190 */ { 3, s_3_190, -1, 1, 0},
/*191 */ { 5, s_3_191, -1, 1, 0},
/*192 */ { 5, s_3_192, -1, 1, 0},
/*193 */ { 5, s_3_193, -1, 1, 0},
/*194 */ { 3, s_3_194, -1, 1, 0},
/*195 */ { 4, s_3_195, -1, 1, 0},
/*196 */ { 4, s_3_196, -1, 1, 0},
/*197 */ { 4, s_3_197, -1, 1, 0},
/*198 */ { 6, s_3_198, 197, 1, 0},
/*199 */ { 6, s_3_199, 197, 1, 0},
/*200 */ { 7, s_3_200, 197, 1, 0},
/*201 */ { 5, s_3_201, 197, 1, 0},
/*202 */ { 7, s_3_202, 201, 1, 0},
/*203 */ { 7, s_3_203, 201, 1, 0},
/*204 */ { 7, s_3_204, 201, 1, 0},
/*205 */ { 6, s_3_205, -1, 1, 0},
/*206 */ { 6, s_3_206, -1, 1, 0},
/*207 */ { 6, s_3_207, -1, 1, 0},
/*208 */ { 6, s_3_208, -1, 1, 0},
/*209 */ { 7, s_3_209, -1, 1, 0},
/*210 */ { 4, s_3_210, -1, 1, 0},
/*211 */ { 5, s_3_211, -1, 1, 0},
/*212 */ { 3, s_3_212, -1, 1, 0},
/*213 */ { 5, s_3_213, 212, 1, 0},
/*214 */ { 3, s_3_214, -1, 1, 0},
/*215 */ { 3, s_3_215, -1, 1, 0},
/*216 */ { 3, s_3_216, -1, 1, 0},
/*217 */ { 4, s_3_217, -1, 1, 0},
/*218 */ { 2, s_3_218, -1, 1, 0},
/*219 */ { 4, s_3_219, 218, 1, 0},
/*220 */ { 4, s_3_220, 218, 1, 0},
/*221 */ { 4, s_3_221, -1, 1, 0},
/*222 */ { 4, s_3_222, -1, 1, 0},
/*223 */ { 4, s_3_223, -1, 1, 0},
/*224 */ { 2, s_3_224, -1, 1, 0},
/*225 */ { 4, s_3_225, 224, 1, 0},
/*226 */ { 2, s_3_226, -1, 1, 0},
/*227 */ { 3, s_3_227, -1, 1, 0},
/*228 */ { 2, s_3_228, -1, 1, 0},
/*229 */ { 2, s_3_229, -1, 1, 0},
/*230 */ { 3, s_3_230, -1, 1, 0},
/*231 */ { 3, s_3_231, -1, 1, 0},
/*232 */ { 3, s_3_232, -1, 1, 0},
/*233 */ { 2, s_3_233, -1, 1, 0},
/*234 */ { 2, s_3_234, -1, 1, 0},
/*235 */ { 2, s_3_235, -1, 1, 0},
/*236 */ { 4, s_3_236, 235, 1, 0},
/*237 */ { 3, s_3_237, -1, 1, 0},
/*238 */ { 4, s_3_238, -1, 1, 0},
/*239 */ { 4, s_3_239, -1, 1, 0},
/*240 */ { 4, s_3_240, -1, 1, 0},
/*241 */ { 4, s_3_241, -1, 1, 0},
/*242 */ { 4, s_3_242, -1, 1, 0},
/*243 */ { 5, s_3_243, -1, 1, 0},
/*244 */ { 5, s_3_244, -1, 1, 0},
/*245 */ { 7, s_3_245, 244, 1, 0},
/*246 */ { 5, s_3_246, -1, 1, 0},
/*247 */ { 5, s_3_247, -1, 1, 0},
/*248 */ { 5, s_3_248, -1, 1, 0},
/*249 */ { 5, s_3_249, -1, 1, 0},
/*250 */ { 4, s_3_250, -1, 1, 0},
/*251 */ { 4, s_3_251, -1, 1, 0},
/*252 */ { 5, s_3_252, -1, 1, 0},
/*253 */ { 3, s_3_253, -1, 1, 0},
/*254 */ { 5, s_3_254, 253, 1, 0},
/*255 */ { 3, s_3_255, -1, 1, 0},
/*256 */ { 5, s_3_256, 255, 1, 0},
/*257 */ { 5, s_3_257, 255, 1, 0},
/*258 */ { 5, s_3_258, -1, 1, 0},
/*259 */ { 5, s_3_259, -1, 1, 0},
/*260 */ { 5, s_3_260, -1, 1, 0},
/*261 */ { 5, s_3_261, -1, 1, 0},
/*262 */ { 5, s_3_262, -1, 1, 0},
/*263 */ { 5, s_3_263, -1, 1, 0},
/*264 */ { 2, s_3_264, -1, 1, 0},
/*265 */ { 2, s_3_265, -1, 1, 0},
/*266 */ { 3, s_3_266, 265, 1, 0},
/*267 */ { 2, s_3_267, -1, 1, 0},
/*268 */ { 3, s_3_268, -1, 1, 0},
/*269 */ { 2, s_3_269, -1, 1, 0},
/*270 */ { 3, s_3_270, -1, 1, 0},
/*271 */ { 3, s_3_271, -1, 1, 0},
/*272 */ { 4, s_3_272, -1, 1, 0},
/*273 */ { 3, s_3_273, -1, 1, 0},
/*274 */ { 3, s_3_274, -1, 1, 0},
/*275 */ { 3, s_3_275, -1, 1, 0},
/*276 */ { 3, s_3_276, -1, 1, 0},
/*277 */ { 3, s_3_277, -1, 1, 0},
/*278 */ { 3, s_3_278, -1, 1, 0},
/*279 */ { 3, s_3_279, -1, 1, 0},
/*280 */ { 1, s_3_280, -1, 1, 0},
/*281 */ { 2, s_3_281, -1, 1, 0},
/*282 */ { 2, s_3_282, -1, 1, 0}
};

static const symbol s_4_0[1] = { 'a' };
static const symbol s_4_1[1] = { 'e' };
static const symbol s_4_2[1] = { 'i' };
static const symbol s_4_3[2] = { 0xEF, 'n' };
static const symbol s_4_4[1] = { 'o' };
static const symbol s_4_5[2] = { 'i', 'r' };
static const symbol s_4_6[1] = { 's' };
static const symbol s_4_7[2] = { 'i', 's' };
static const symbol s_4_8[2] = { 'o', 's' };
static const symbol s_4_9[2] = { 0xEF, 's' };
static const symbol s_4_10[2] = { 'i', 't' };
static const symbol s_4_11[2] = { 'e', 'u' };
static const symbol s_4_12[2] = { 'i', 'u' };
static const symbol s_4_13[3] = { 'i', 'q', 'u' };
static const symbol s_4_14[3] = { 'i', 't', 'z' };
static const symbol s_4_15[1] = { 0xE0 };
static const symbol s_4_16[1] = { 0xE1 };
static const symbol s_4_17[1] = { 0xE9 };
static const symbol s_4_18[1] = { 0xEC };
static const symbol s_4_19[1] = { 0xED };
static const symbol s_4_20[1] = { 0xEF };
static const symbol s_4_21[1] = { 0xF3 };

static const struct among a_4[22] =
{
/*  0 */ { 1, s_4_0, -1, 1, 0},
/*  1 */ { 1, s_4_1, -1, 1, 0},
/*  2 */ { 1, s_4_2, -1, 1, 0},
/*  3 */ { 2, s_4_3, -1, 1, 0},
/*  4 */ { 1, s_4_4, -1, 1, 0},
/*  5 */ { 2, s_4_5, -1, 1, 0},
/*  6 */ { 1, s_4_6, -1, 1, 0},
/*  7 */ { 2, s_4_7, 6, 1, 0},
/*  8 */ { 2, s_4_8, 6, 1, 0},
/*  9 */ { 2, s_4_9, 6, 1, 0},
/* 10 */ { 2, s_4_10, -1, 1, 0},
/* 11 */ { 2, s_4_11, -1, 1, 0},
/* 12 */ { 2, s_4_12, -1, 1, 0},
/* 13 */ { 3, s_4_13, -1, 2, 0},
/* 14 */ { 3, s_4_14, -1, 1, 0},
/* 15 */ { 1, s_4_15, -1, 1, 0},
/* 16 */ { 1, s_4_16, -1, 1, 0},
/* 17 */ { 1, s_4_17, -1, 1, 0},
/* 18 */ { 1, s_4_18, -1, 1, 0},
/* 19 */ { 1, s_4_19, -1, 1, 0},
/* 20 */ { 1, s_4_20, -1, 1, 0},
/* 21 */ { 1, s_4_21, -1, 1, 0}
};

static const unsigned char g_v[] = { 17, 65, 16, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 128, 129, 81, 6, 10 };

static const symbol s_0[] = { 'a' };
static const symbol s_1[] = { 'a' };
static const symbol s_2[] = { 'e' };
static const symbol s_3[] = { 'e' };
static const symbol s_4[] = { 'i' };
static const symbol s_5[] = { 'i' };
static const symbol s_6[] = { 'o' };
static const symbol s_7[] = { 'o' };
static const symbol s_8[] = { 'u' };
static const symbol s_9[] = { 'u' };
static const symbol s_10[] = { 'i' };
static const symbol s_11[] = { '.' };
static const symbol s_12[] = { 'l', 'o', 'g' };
static const symbol s_13[] = { 'i', 'c' };
static const symbol s_14[] = { 'c' };
static const symbol s_15[] = { 'i', 'c' };

static int r_mark_regions(struct SN_env * z) { /* forwardmode */
    z->I[0] = z->l; /* $p1 = <integer expression>, line 39 */
    z->I[1] = z->l; /* $p2 = <integer expression>, line 40 */
    {   int c1 = z->c; /* do, line 42 */
        {    /* gopast */ /* grouping v, line 43 */
            int ret = out_grouping(z, g_v, 97, 252, 1);
            if (ret < 0) goto lab0;
            z->c += ret;
        }
        {    /* gopast */ /* non v, line 43 */
            int ret = in_grouping(z, g_v, 97, 252, 1);
            if (ret < 0) goto lab0;
            z->c += ret;
        }
        z->I[0] = z->c; /* setmark p1, line 43 */
        {    /* gopast */ /* grouping v, line 44 */
            int ret = out_grouping(z, g_v, 97, 252, 1);
            if (ret < 0) goto lab0;
            z->c += ret;
        }
        {    /* gopast */ /* non v, line 44 */
            int ret = in_grouping(z, g_v, 97, 252, 1);
            if (ret < 0) goto lab0;
            z->c += ret;
        }
        z->I[1] = z->c; /* setmark p2, line 44 */
    lab0:
        z->c = c1;
    }
    return 1;
}

static int r_cleaning(struct SN_env * z) { /* forwardmode */
    int among_var;
    while(1) { /* repeat, line 48 */
        int c1 = z->c;
        z->bra = z->c; /* [, line 49 */
        among_var = find_among(z, a_0, 13); /* substring, line 49 */
        if (!(among_var)) goto lab0;
        z->ket = z->c; /* ], line 49 */
        switch (among_var) { /* among, line 49 */
            case 0: goto lab0;
            case 1:
                {   int ret = slice_from_s(z, 1, s_0); /* <-, line 50 */
                    if (ret < 0) return ret;
                }
                break;
            case 2:
                {   int ret = slice_from_s(z, 1, s_1); /* <-, line 51 */
                    if (ret < 0) return ret;
                }
                break;
            case 3:
                {   int ret = slice_from_s(z, 1, s_2); /* <-, line 52 */
                    if (ret < 0) return ret;
                }
                break;
            case 4:
                {   int ret = slice_from_s(z, 1, s_3); /* <-, line 53 */
                    if (ret < 0) return ret;
                }
                break;
            case 5:
                {   int ret = slice_from_s(z, 1, s_4); /* <-, line 54 */
                    if (ret < 0) return ret;
                }
                break;
            case 6:
                {   int ret = slice_from_s(z, 1, s_5); /* <-, line 55 */
                    if (ret < 0) return ret;
                }
                break;
            case 7:
                {   int ret = slice_from_s(z, 1, s_6); /* <-, line 56 */
                    if (ret < 0) return ret;
                }
                break;
            case 8:
                {   int ret = slice_from_s(z, 1, s_7); /* <-, line 57 */
                    if (ret < 0) return ret;
                }
                break;
            case 9:
                {   int ret = slice_from_s(z, 1, s_8); /* <-, line 58 */
                    if (ret < 0) return ret;
                }
                break;
            case 10:
                {   int ret = slice_from_s(z, 1, s_9); /* <-, line 59 */
                    if (ret < 0) return ret;
                }
                break;
            case 11:
                {   int ret = slice_from_s(z, 1, s_10); /* <-, line 60 */
                    if (ret < 0) return ret;
                }
                break;
            case 12:
                {   int ret = slice_from_s(z, 1, s_11); /* <-, line 61 */
                    if (ret < 0) return ret;
                }
                break;
            case 13:
                if (z->c >= z->l) goto lab0;
                z->c++; /* next, line 62 */
                break;
        }
        continue;
    lab0:
        z->c = c1;
        break;
    }
    return 1;
}

static int r_R1(struct SN_env * z) { /* backwardmode */
    if (!(z->I[0] <= z->c)) return 0; /* $p1 <= <integer expression>, line 68 */
    return 1;
}

static int r_R2(struct SN_env * z) { /* backwardmode */
    if (!(z->I[1] <= z->c)) return 0; /* $p2 <= <integer expression>, line 69 */
    return 1;
}

static int r_attached_pronoun(struct SN_env * z) { /* backwardmode */
    int among_var;
    z->ket = z->c; /* [, line 72 */
    if (z->c - 1 <= z->lb || z->p[z->c - 1] >> 5 != 3 || !((1634850 >> (z->p[z->c - 1] & 0x1f)) & 1)) return 0; /* substring, line 72 */
    among_var = find_among_b(z, a_1, 39);
    if (!(among_var)) return 0;
    z->bra = z->c; /* ], line 72 */
    switch (among_var) { /* among, line 72 */
        case 0: return 0;
        case 1:
            {   int ret = r_R1(z); /* call R1, line 82 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_del(z); /* delete, line 82 */
                if (ret < 0) return ret;
            }
            break;
    }
    return 1;
}

static int r_standard_suffix(struct SN_env * z) { /* backwardmode */
    int among_var;
    z->ket = z->c; /* [, line 87 */
    among_var = find_among_b(z, a_2, 200); /* substring, line 87 */
    if (!(among_var)) return 0;
    z->bra = z->c; /* ], line 87 */
    switch (among_var) { /* among, line 87 */
        case 0: return 0;
        case 1:
            {   int ret = r_R1(z); /* call R1, line 111 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_del(z); /* delete, line 111 */
                if (ret < 0) return ret;
            }
            break;
        case 2:
            {   int ret = r_R2(z); /* call R2, line 113 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_del(z); /* delete, line 113 */
                if (ret < 0) return ret;
            }
            break;
        case 3:
            {   int ret = r_R2(z); /* call R2, line 115 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_from_s(z, 3, s_12); /* <-, line 115 */
                if (ret < 0) return ret;
            }
            break;
        case 4:
            {   int ret = r_R2(z); /* call R2, line 117 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_from_s(z, 2, s_13); /* <-, line 117 */
                if (ret < 0) return ret;
            }
            break;
        case 5:
            {   int ret = r_R1(z); /* call R1, line 119 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_from_s(z, 1, s_14); /* <-, line 119 */
                if (ret < 0) return ret;
            }
            break;
    }
    return 1;
}

static int r_verb_suffix(struct SN_env * z) { /* backwardmode */
    int among_var;
    z->ket = z->c; /* [, line 124 */
    among_var = find_among_b(z, a_3, 283); /* substring, line 124 */
    if (!(among_var)) return 0;
    z->bra = z->c; /* ], line 124 */
    switch (among_var) { /* among, line 124 */
        case 0: return 0;
        case 1:
            {   int ret = r_R1(z); /* call R1, line 169 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_del(z); /* delete, line 169 */
                if (ret < 0) return ret;
            }
            break;
        case 2:
            {   int ret = r_R2(z); /* call R2, line 171 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_del(z); /* delete, line 171 */
                if (ret < 0) return ret;
            }
            break;
    }
    return 1;
}

static int r_residual_suffix(struct SN_env * z) { /* backwardmode */
    int among_var;
    z->ket = z->c; /* [, line 176 */
    among_var = find_among_b(z, a_4, 22); /* substring, line 176 */
    if (!(among_var)) return 0;
    z->bra = z->c; /* ], line 176 */
    switch (among_var) { /* among, line 176 */
        case 0: return 0;
        case 1:
            {   int ret = r_R1(z); /* call R1, line 179 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_del(z); /* delete, line 179 */
                if (ret < 0) return ret;
            }
            break;
        case 2:
            {   int ret = r_R1(z); /* call R1, line 181 */
                if (ret <= 0) return ret;
            }
            {   int ret = slice_from_s(z, 2, s_15); /* <-, line 181 */
                if (ret < 0) return ret;
            }
            break;
    }
    return 1;
}

extern int stem(struct SN_env * z) { /* forwardmode */
    {   int c1 = z->c; /* do, line 187 */
        {   int ret = r_mark_regions(z); /* call mark_regions, line 187 */
            if (ret == 0) goto lab0;
            if (ret < 0) return ret;
        }
    lab0:
        z->c = c1;
    }
    z->lb = z->c; z->c = z->l; /* backwards, line 188 */

    {   int m2 = z->l - z->c; (void)m2; /* do, line 189 */
        {   int ret = r_attached_pronoun(z); /* call attached_pronoun, line 189 */
            if (ret == 0) goto lab1;
            if (ret < 0) return ret;
        }
    lab1:
        z->c = z->l - m2;
    }
    {   int m3 = z->l - z->c; (void)m3; /* do, line 190 */
        {   int m4 = z->l - z->c; (void)m4; /* or, line 190 */
            {   int ret = r_standard_suffix(z); /* call standard_suffix, line 190 */
                if (ret == 0) goto lab4;
                if (ret < 0) return ret;
            }
            goto lab3;
        lab4:
            z->c = z->l - m4;
            {   int ret = r_verb_suffix(z); /* call verb_suffix, line 191 */
                if (ret == 0) goto lab2;
                if (ret < 0) return ret;
            }
        }
    lab3:
    lab2:
        z->c = z->l - m3;
    }
    {   int m5 = z->l - z->c; (void)m5; /* do, line 193 */
        {   int ret = r_residual_suffix(z); /* call residual_suffix, line 193 */
            if (ret == 0) goto lab5;
            if (ret < 0) return ret;
        }
    lab5:
        z->c = z->l - m5;
    }
    z->c = z->lb;
    {   int c6 = z->c; /* do, line 195 */
        {   int ret = r_cleaning(z); /* call cleaning, line 195 */
            if (ret == 0) goto lab6;
            if (ret < 0) return ret;
        }
    lab6:
        z->c = c6;
    }
    return 1;
}

extern struct SN_env * create_env(void) { return SN_create_env(0, 2, 0); }

extern void close_env(struct SN_env * z) { SN_close_env(z, 0); }

