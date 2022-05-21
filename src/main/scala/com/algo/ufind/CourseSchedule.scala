package com.algo.ufind

import scala.collection.immutable

object CourseSchedule extends App {
  val num = 1000
  //  val p: Array[Array[Int]] = Array.ofDim[Int](num, 2)
  //    p(0) = Array(1, 0)
  //    p(1) = Array(2, 0)
  //    p(2) = Array(3, 1)
  //    p(3) = Array(3, 2)
  //  p(0) = Array(1, 0)
  //  p(1) = Array(0, 1)
  //  p(2) = Array(2, 3)
  //  p(3) = Array(2, 0)
  val p = Array(Array(785, 230), Array(843, 838), Array(725, 91), Array(236, 135), Array(804, 544), Array(779, 204), Array(599, 306), Array(685, 651), Array(716, 562), Array(419, 381), Array(575, 549), Array(895, 348), Array(872, 16), Array(938, 344), Array(565, 340), Array(794, 21), Array(867, 557), Array(857, 486), Array(256, 131), Array(959, 439), Array(756, 728), Array(873, 330), Array(320, 99), Array(825, 657), Array(620, 63), Array(534, 404), Array(795, 385), Array(171, 159), Array(982, 854), Array(458, 203), Array(243, 107), Array(403, 289), Array(868, 400), Array(313, 214), Array(851, 368), Array(773, 767), Array(276, 0), Array(948, 672), Array(439, 100), Array(437, 255), Array(272, 175), Array(758, 158), Array(495, 453), Array(480, 158), Array(240, 61), Array(970, 568), Array(221, 215), Array(758, 22), Array(310, 106), Array(822, 111), Array(229, 163), Array(386, 150), Array(293, 94), Array(950, 25), Array(959, 680), Array(858, 78), Array(819, 512), Array(672, 385), Array(830, 353), Array(961, 919), Array(757, 507), Array(180, 98), Array(755, 237), Array(382, 308), Array(502, 260), Array(987, 407), Array(834, 646), Array(963, 895), Array(348, 320), Array(973, 436), Array(96, 32), Array(916, 857), Array(373, 287), Array(948, 205), Array(277, 84), Array(467, 386), Array(663, 289), Array(763, 152), Array(788, 323), Array(958, 514), Array(757, 675), Array(980, 387), Array(494, 78), Array(883, 245), Array(974, 615), Array(467, 153), Array(763, 40), Array(732, 626), Array(355, 244), Array(751, 586), Array(839, 11), Array(675, 13), Array(52, 19), Array(853, 6), Array(758, 296), Array(534, 339), Array(898, 550), Array(744, 59), Array(822, 166), Array(338, 20), Array(730, 149), Array(979, 725), Array(539, 188), Array(848, 413), Array(798, 115), Array(399, 215), Array(832, 268), Array(709, 319), Array(894, 175), Array(998, 373), Array(858, 480), Array(263, 189), Array(522, 25), Array(613, 377), Array(736, 602), Array(253, 94), Array(375, 128), Array(374, 54), Array(929, 463), Array(722, 265), Array(435, 261), Array(841, 780), Array(585, 324), Array(46, 14), Array(972, 142), Array(811, 715), Array(514, 142), Array(240, 142), Array(423, 412), Array(856, 140), Array(643, 149), Array(570, 399), Array(491, 390), Array(498, 303), Array(919, 514), Array(616, 488), Array(497, 447), Array(235, 179), Array(362, 83), Array(913, 323), Array(767, 502), Array(470, 336), Array(398, 2), Array(702, 16), Array(420, 3), Array(670, 74), Array(942, 107), Array(579, 513), Array(466, 252), Array(775, 608), Array(321, 251), Array(653, 22), Array(955, 743), Array(923, 64), Array(443, 48), Array(817, 152), Array(288, 180), Array(983, 660), Array(223, 85), Array(816, 341), Array(812, 247), Array(733, 264), Array(610, 204), Array(761, 400), Array(652, 592), Array(840, 39), Array(929, 325), Array(814, 203), Array(477, 373), Array(888, 337), Array(722, 92), Array(980, 260), Array(532, 181), Array(886, 639), Array(784, 421), Array(962, 531), Array(784, 57), Array(423, 30), Array(277, 172), Array(558, 349), Array(781, 403), Array(998, 216), Array(864, 741), Array(941, 901), Array(996, 413), Array(944, 400), Array(994, 394), Array(781, 612), Array(607, 206), Array(453, 261), Array(462, 160), Array(534, 406), Array(637, 4), Array(897, 713), Array(867, 244), Array(463, 33), Array(473, 462), Array(149, 35), Array(78, 39), Array(479, 91), Array(832, 280), Array(923, 717), Array(447, 147), Array(254, 141), Array(332, 121), Array(698, 196), Array(576, 233), Array(914, 272), Array(891, 149), Array(447, 140), Array(719, 449), Array(343, 274), Array(935, 268), Array(495, 156), Array(433, 59), Array(969, 526), Array(752, 347), Array(948, 291), Array(734, 357), Array(795, 218), Array(274, 47), Array(863, 32), Array(427, 289), Array(969, 859), Array(453, 46), Array(428, 212), Array(702, 152), Array(773, 435), Array(539, 504), Array(318, 14), Array(733, 141), Array(307, 268), Array(499, 482), Array(713, 189), Array(706, 325), Array(490, 349), Array(970, 120), Array(892, 72), Array(942, 484), Array(771, 718), Array(784, 325), Array(665, 543), Array(530, 474), Array(827, 90), Array(564, 360), Array(247, 172), Array(610, 374), Array(853, 700), Array(364, 233), Array(893, 425), Array(695, 530), Array(382, 42), Array(779, 582), Array(472, 17), Array(988, 543), Array(498, 214), Array(895, 532), Array(851, 165), Array(960, 845), Array(825, 470), Array(626, 373), Array(891, 186), Array(830, 658), Array(986, 925), Array(541, 210), Array(633, 606), Array(847, 634), Array(538, 454), Array(673, 25), Array(417, 140), Array(774, 42), Array(275, 143), Array(608, 495), Array(736, 274), Array(687, 403), Array(882, 434), Array(729, 345), Array(773, 502), Array(955, 184), Array(932, 127), Array(577, 524), Array(705, 289), Array(918, 28), Array(506, 441), Array(663, 352), Array(566, 530), Array(256, 255), Array(206, 64), Array(511, 256), Array(771, 393), Array(732, 279), Array(489, 464), Array(727, 451), Array(653, 505), Array(921, 749), Array(915, 537), Array(906, 90), Array(931, 420), Array(970, 543), Array(624, 537), Array(354, 126), Array(554, 373), Array(699, 476), Array(901, 109), Array(600, 388), Array(179, 81), Array(829, 205), Array(824, 549), Array(987, 861), Array(578, 538), Array(356, 147), Array(666, 287), Array(932, 264), Array(392, 138), Array(779, 98), Array(715, 320), Array(564, 509), Array(646, 293), Array(294, 53), Array(706, 472), Array(548, 512), Array(905, 860), Array(926, 804), Array(715, 323), Array(788, 547), Array(655, 419), Array(813, 451), Array(528, 482), Array(779, 93), Array(908, 193), Array(463, 200), Array(847, 284), Array(231, 128), Array(620, 361), Array(372, 169), Array(435, 257), Array(394, 268), Array(420, 368), Array(850, 130), Array(631, 144), Array(657, 63), Array(423, 222), Array(580, 116), Array(382, 334), Array(385, 242), Array(265, 125), Array(325, 125), Array(209, 71), Array(519, 274), Array(917, 870), Array(779, 742), Array(751, 604), Array(839, 281), Array(483, 287), Array(256, 34), Array(666, 389), Array(913, 135), Array(513, 161), Array(666, 170), Array(426, 425), Array(804, 631), Array(461, 280), Array(507, 156), Array(758, 73), Array(764, 306), Array(905, 499), Array(234, 86), Array(630, 252), Array(876, 124), Array(318, 275), Array(331, 301), Array(874, 190), Array(969, 681), Array(862, 302), Array(885, 794), Array(616, 206), Array(699, 142), Array(877, 538), Array(802, 475), Array(704, 424), Array(479, 300), Array(916, 326), Array(932, 573), Array(845, 230), Array(224, 46), Array(790, 401), Array(795, 700), Array(639, 176), Array(917, 471), Array(652, 217), Array(944, 928), Array(518, 110), Array(661, 103), Array(874, 186), Array(206, 72), Array(730, 404), Array(969, 674), Array(801, 770), Array(535, 1), Array(390, 243), Array(270, 211), Array(626, 545), Array(875, 464), Array(601, 484), Array(630, 505), Array(195, 82), Array(891, 703), Array(545, 411), Array(603, 252), Array(215, 177), Array(729, 440), Array(951, 351), Array(476, 42), Array(876, 261), Array(812, 248), Array(735, 110), Array(386, 102), Array(534, 72), Array(556, 310), Array(326, 282), Array(608, 226), Array(900, 17), Array(667, 555), Array(794, 164), Array(709, 304), Array(907, 297), Array(421, 62), Array(608, 102), Array(289, 58), Array(383, 84), Array(895, 254), Array(694, 156), Array(968, 311), Array(932, 139), Array(657, 67), Array(936, 659), Array(875, 759), Array(483, 233), Array(490, 469), Array(84, 40), Array(321, 225), Array(843, 341), Array(449, 255), Array(719, 325), Array(854, 271), Array(697, 47), Array(594, 320), Array(734, 253), Array(655, 552), Array(302, 154), Array(704, 624), Array(674, 308), Array(790, 609), Array(883, 427), Array(549, 417), Array(972, 196), Array(933, 119), Array(999, 40), Array(760, 745), Array(543, 84), Array(994, 19), Array(506, 343), Array(440, 121), Array(947, 54), Array(713, 289), Array(642, 210), Array(810, 410), Array(820, 808), Array(964, 684), Array(963, 709), Array(347, 292), Array(973, 503), Array(943, 204), Array(728, 577), Array(851, 741), Array(549, 215), Array(415, 40), Array(957, 439), Array(279, 181), Array(931, 883), Array(840, 84), Array(742, 555), Array(803, 196), Array(884, 332), Array(708, 352), Array(643, 192), Array(976, 278), Array(693, 665), Array(661, 462), Array(792, 538), Array(961, 238), Array(133, 107), Array(561, 180), Array(860, 508), Array(924, 616), Array(870, 660), Array(776, 369), Array(793, 307), Array(944, 868), Array(856, 56), Array(415, 376), Array(444, 397), Array(628, 426), Array(790, 45), Array(304, 108), Array(457, 349), Array(441, 161), Array(515, 94), Array(899, 109), Array(867, 195), Array(718, 404), Array(619, 462), Array(103, 33), Array(862, 77), Array(727, 672), Array(825, 815), Array(722, 556), Array(934, 883), Array(828, 322), Array(630, 368), Array(282, 170), Array(824, 371), Array(851, 779), Array(691, 155), Array(533, 382), Array(884, 61), Array(697, 130), Array(201, 105), Array(798, 296), Array(855, 265), Array(503, 31), Array(388, 124), Array(850, 152), Array(933, 203), Array(736, 80), Array(335, 239), Array(446, 6), Array(630, 178), Array(802, 88), Array(299, 270), Array(696, 526), Array(869, 269), Array(659, 292), Array(284, 93), Array(719, 212), Array(710, 480), Array(497, 205), Array(565, 123), Array(547, 442), Array(582, 385), Array(670, 550), Array(729, 218), Array(731, 285), Array(405, 57), Array(734, 683), Array(424, 102), Array(823, 39), Array(665, 629), Array(392, 24), Array(802, 242), Array(791, 620), Array(864, 112), Array(178, 94), Array(898, 511), Array(978, 595), Array(887, 3), Array(521, 144), Array(515, 178), Array(908, 302), Array(233, 60), Array(735, 161), Array(347, 241), Array(736, 117), Array(233, 192), Array(633, 449), Array(831, 806), Array(308, 304), Array(868, 425), Array(307, 33), Array(907, 67), Array(501, 265), Array(945, 62), Array(516, 88), Array(922, 119), Array(358, 150), Array(969, 680), Array(787, 500), Array(400, 366), Array(662, 569), Array(767, 316), Array(821, 38), Array(945, 13), Array(871, 413), Array(536, 251), Array(647, 591), Array(645, 84), Array(418, 242), Array(153, 37), Array(647, 32), Array(867, 298), Array(667, 203), Array(988, 398), Array(890, 578), Array(988, 712), Array(454, 369), Array(420, 92), Array(644, 631), Array(458, 215), Array(921, 153), Array(913, 490), Array(947, 594), Array(774, 121), Array(286, 212), Array(489, 8), Array(289, 236), Array(809, 664), Array(793, 694), Array(940, 395), Array(647, 441), Array(953, 352), Array(521, 194), Array(924, 592), Array(885, 842), Array(199, 149), Array(489, 243), Array(736, 692), Array(424, 329), Array(788, 9), Array(606, 457), Array(685, 615), Array(405, 403), Array(604, 151), Array(553, 430), Array(876, 372), Array(920, 598), Array(939, 666), Array(993, 109), Array(708, 115), Array(760, 153), Array(753, 455), Array(125, 13), Array(688, 153), Array(883, 161), Array(769, 427), Array(960, 308), Array(824, 817), Array(479, 169), Array(829, 163), Array(773, 280), Array(735, 612), Array(938, 610), Array(914, 378), Array(832, 126), Array(920, 715), Array(653, 429), Array(849, 412), Array(542, 103), Array(668, 47), Array(917, 603), Array(504, 271), Array(760, 399), Array(751, 7), Array(945, 327), Array(46, 19), Array(316, 176), Array(411, 38), Array(666, 136), Array(818, 384), Array(932, 108), Array(972, 658), Array(666, 23), Array(868, 518), Array(690, 258), Array(241, 226), Array(553, 535), Array(837, 269), Array(988, 619), Array(983, 285), Array(635, 259), Array(800, 110), Array(573, 65), Array(841, 318), Array(531, 424), Array(705, 77), Array(474, 154), Array(757, 552), Array(606, 18), Array(871, 735), Array(961, 623), Array(605, 13), Array(900, 121), Array(180, 7), Array(906, 737), Array(862, 495), Array(582, 443), Array(233, 197), Array(629, 430), Array(370, 32), Array(890, 864), Array(879, 659), Array(778, 248), Array(988, 434), Array(521, 109), Array(903, 668), Array(891, 809), Array(529, 247), Array(794, 137), Array(663, 648), Array(988, 376), Array(924, 712), Array(640, 319), Array(409, 348), Array(780, 447), Array(918, 327), Array(570, 103), Array(473, 203), Array(743, 469), Array(445, 282), Array(965, 926), Array(720, 359), Array(862, 520), Array(772, 130), Array(319, 192), Array(703, 75), Array(954, 172), Array(885, 208), Array(629, 484), Array(775, 587), Array(919, 260), Array(835, 395), Array(707, 681), Array(826, 105), Array(991, 840), Array(638, 409), Array(965, 125), Array(949, 289), Array(809, 408), Array(923, 423), Array(853, 340), Array(781, 748), Array(519, 0), Array(317, 172), Array(723, 49), Array(909, 580), Array(780, 62), Array(830, 789), Array(813, 784), Array(903, 249), Array(570, 440), Array(631, 625), Array(720, 32), Array(381, 122), Array(406, 112), Array(865, 620), Array(736, 497), Array(730, 280), Array(417, 35), Array(638, 40), Array(767, 531), Array(679, 147), Array(485, 19), Array(905, 554), Array(897, 651), Array(491, 139), Array(731, 722), Array(627, 516), Array(486, 107), Array(766, 184), Array(793, 223), Array(857, 489), Array(576, 156), Array(878, 784), Array(979, 892), Array(611, 501), Array(964, 651), Array(367, 231), Array(183, 59), Array(905, 242), Array(487, 45), Array(912, 534), Array(623, 367), Array(901, 692), Array(882, 816), Array(407, 275), Array(788, 230), Array(887, 286), Array(652, 341), Array(915, 73), Array(303, 45), Array(960, 304), Array(705, 316), Array(707, 314), Array(920, 351), Array(956, 48), Array(818, 814), Array(529, 349), Array(507, 236), Array(764, 206), Array(911, 217), Array(429, 323), Array(903, 487), Array(642, 376), Array(763, 329), Array(175, 66), Array(823, 171), Array(856, 395), Array(590, 106), Array(384, 323), Array(616, 561), Array(792, 785), Array(887, 387), Array(823, 431), Array(521, 418), Array(161, 101), Array(929, 367), Array(612, 352), Array(859, 549), Array(810, 147), Array(314, 13), Array(920, 71), Array(783, 366), Array(525, 399), Array(300, 19), Array(653, 593), Array(165, 99), Array(772, 349), Array(804, 209), Array(926, 156), Array(335, 241), Array(465, 294), Array(567, 502), Array(365, 167), Array(689, 651), Array(684, 403), Array(584, 1), Array(902, 283), Array(630, 311), Array(743, 156), Array(666, 660), Array(114, 90), Array(542, 237), Array(606, 229), Array(609, 206), Array(514, 220), Array(931, 650), Array(928, 398), Array(892, 513), Array(591, 276), Array(685, 406), Array(185, 88), Array(566, 357), Array(875, 495), Array(798, 417), Array(286, 13), Array(708, 410), Array(748, 375), Array(970, 403), Array(966, 910), Array(943, 514), Array(678, 433), Array(933, 876), Array(489, 423), Array(337, 143), Array(710, 491), Array(932, 59), Array(972, 907), Array(399, 174), Array(396, 44), Array(349, 298), Array(355, 230), Array(719, 608), Array(682, 113), Array(333, 172), Array(930, 783), Array(916, 344), Array(514, 92), Array(614, 153), Array(676, 218), Array(442, 425), Array(556, 266), Array(851, 381), Array(683, 247), Array(285, 83), Array(939, 556), Array(221, 35), Array(546, 109), Array(183, 144), Array(827, 506), Array(737, 64), Array(488, 433), Array(756, 294), Array(842, 17), Array(839, 37), Array(881, 256), Array(689, 317), Array(974, 592), Array(488, 225), Array(603, 342), Array(649, 301), Array(641, 478), Array(904, 574), Array(437, 57), Array(898, 59), Array(509, 478), Array(479, 144), Array(78, 16), Array(231, 4), Array(677, 298), Array(943, 58), Array(632, 14), Array(699, 281), Array(852, 308), Array(251, 211), Array(448, 333), Array(361, 289), Array(837, 397), Array(380, 60), Array(848, 340), Array(981, 531), Array(219, 83), Array(301, 112), Array(993, 36), Array(948, 22), Array(843, 830), Array(858, 358), Array(491, 392), Array(696, 45), Array(983, 664), Array(886, 456), Array(557, 521), Array(919, 674), Array(180, 36), Array(638, 291), Array(951, 661), Array(597, 283), Array(950, 881), Array(515, 288), Array(531, 418), Array(514, 499), Array(254, 102), Array(350, 215), Array(509, 25), Array(610, 71), Array(533, 18), Array(453, 42), Array(157, 120), Array(626, 434), Array(800, 671), Array(900, 211), Array(829, 626), Array(699, 269), Array(415, 343), Array(677, 116), Array(521, 82), Array(568, 8), Array(492, 454), Array(318, 197), Array(645, 131), Array(675, 360), Array(795, 563), Array(254, 129), Array(793, 149), Array(709, 512), Array(591, 138), Array(411, 148), Array(127, 25), Array(154, 9), Array(470, 87), Array(852, 660), Array(739, 60), Array(875, 280), Array(701, 332), Array(288, 234), Array(923, 74), Array(909, 362), Array(870, 330), Array(869, 647), Array(883, 338), Array(711, 246), Array(196, 30), Array(716, 393), Array(811, 201), Array(970, 808), Array(607, 398), Array(755, 732), Array(528, 431), Array(936, 237), Array(633, 406), Array(950, 922), Array(129, 53), Array(904, 891), Array(633, 589), Array(470, 129), Array(432, 386), Array(688, 35), Array(798, 663), Array(319, 202), Array(791, 335), Array(425, 63), Array(937, 570), Array(170, 19), Array(298, 262), Array(714, 194), Array(827, 797), Array(913, 824), Array(584, 483), Array(738, 234), Array(448, 161), Array(942, 931), Array(495, 485), Array(810, 683), Array(885, 228), Array(213, 159), Array(983, 716), Array(611, 172), Array(830, 138), Array(389, 332), Array(970, 166), Array(251, 21), Array(669, 555), Array(439, 33), Array(381, 204), Array(881, 724), Array(843, 842), Array(640, 449), Array(183, 32), Array(384, 66), Array(557, 305), Array(462, 424), Array(841, 676), Array(888, 39), Array(777, 511), Array(994, 798), Array(613, 81), Array(922, 574), Array(442, 330), Array(185, 50), Array(976, 48), Array(428, 119), Array(994, 863), Array(473, 182), Array(786, 506), Array(807, 497), Array(668, 276), Array(308, 195), Array(644, 65), Array(632, 188), Array(673, 338), Array(402, 300), Array(894, 336), Array(420, 96), Array(732, 318), Array(475, 402), Array(969, 636), Array(728, 487), Array(370, 163), Array(648, 638), Array(997, 674), Array(929, 386), Array(341, 167), Array(890, 564), Array(732, 368), Array(711, 644), Array(876, 834), Array(152, 150), Array(139, 138), Array(834, 185), Array(570, 185), Array(966, 26), Array(816, 101), Array(880, 642), Array(602, 92), Array(907, 463), Array(435, 367), Array(405, 154), Array(256, 20), Array(820, 36), Array(411, 194), Array(103, 95), Array(929, 59), Array(648, 343), Array(858, 22), Array(995, 335), Array(836, 215), Array(830, 624), Array(460, 326), Array(802, 462), Array(841, 708), Array(892, 273), Array(784, 386), Array(760, 5), Array(834, 602), Array(963, 887), Array(327, 50), Array(630, 112), Array(809, 328), Array(659, 379), Array(349, 84), Array(485, 69), Array(783, 408), Array(458, 128), Array(976, 155), Array(712, 665), Array(230, 28), Array(526, 495), Array(425, 227), Array(986, 182), Array(299, 162), Array(977, 569), Array(829, 258), Array(603, 221), Array(255, 28), Array(717, 519), Array(924, 404), Array(384, 310), Array(804, 552), Array(880, 821), Array(895, 477), Array(408, 285), Array(236, 53), Array(998, 355), Array(688, 13), Array(834, 413), Array(316, 245), Array(221, 169), Array(951, 695), Array(926, 641), Array(410, 266), Array(574, 350), Array(707, 263), Array(291, 79), Array(953, 497), Array(493, 455), Array(824, 623), Array(289, 9), Array(818, 749), Array(476, 284), Array(529, 528), Array(825, 158), Array(305, 152), Array(447, 83), Array(304, 182), Array(753, 6), Array(879, 332), Array(596, 452), Array(815, 665), Array(28, 7), Array(751, 657), Array(385, 208), Array(985, 299), Array(186, 31), Array(863, 176), Array(421, 141), Array(743, 467), Array(542, 21), Array(900, 324), Array(723, 466), Array(193, 62), Array(397, 251), Array(805, 167), Array(304, 284), Array(622, 541), Array(665, 301), Array(544, 17), Array(702, 363), Array(746, 709), Array(660, 389), Array(461, 398), Array(249, 214), Array(555, 533), Array(422, 9), Array(967, 32), Array(948, 125), Array(566, 170), Array(773, 766), Array(997, 54), Array(958, 382), Array(163, 68), Array(644, 396), Array(603, 2), Array(850, 36), Array(433, 292), Array(962, 144), Array(712, 466), Array(770, 170), Array(992, 149), Array(351, 198), Array(326, 127), Array(760, 517), Array(892, 258), Array(698, 522), Array(920, 679), Array(608, 479), Array(724, 163), Array(664, 605), Array(559, 17), Array(417, 343), Array(822, 63), Array(715, 578), Array(292, 16), Array(346, 119), Array(826, 798), Array(895, 567), Array(325, 21), Array(661, 318), Array(614, 152), Array(666, 549), Array(233, 30), Array(518, 403), Array(766, 370), Array(379, 220), Array(211, 62), Array(403, 348), Array(208, 55), Array(195, 131), Array(879, 241), Array(351, 336), Array(990, 10), Array(849, 184), Array(720, 49), Array(308, 129), Array(439, 356), Array(934, 132), Array(571, 210), Array(952, 857), Array(845, 376), Array(466, 34), Array(343, 300), Array(915, 737), Array(487, 78), Array(912, 518), Array(620, 418), Array(902, 827), Array(887, 110), Array(551, 6), Array(764, 14), Array(60, 35), Array(230, 139), Array(569, 540), Array(521, 244), Array(260, 42), Array(943, 523), Array(955, 371), Array(505, 20), Array(682, 157), Array(582, 149), Array(723, 63), Array(942, 461), Array(504, 445), Array(933, 733), Array(417, 80), Array(626, 16), Array(952, 509), Array(313, 65), Array(419, 228), Array(462, 125), Array(901, 712), Array(759, 201), Array(770, 417), Array(827, 672), Array(763, 557), Array(571, 243), Array(345, 279), Array(873, 857), Array(659, 298), Array(845, 140), Array(287, 19), Array(774, 484), Array(486, 301), Array(648, 71), Array(522, 375), Array(891, 618), Array(866, 559), Array(460, 412), Array(351, 267), Array(881, 252), Array(914, 843), Array(465, 400), Array(416, 212), Array(97, 21), Array(986, 376), Array(137, 78), Array(820, 770), Array(769, 274), Array(615, 159), Array(858, 176), Array(825, 346), Array(321, 49), Array(809, 97), Array(244, 16), Array(684, 82), Array(666, 379), Array(831, 492), Array(805, 358), Array(814, 80), Array(725, 142), Array(384, 125), Array(383, 327), Array(897, 707), Array(575, 445), Array(616, 330), Array(913, 463), Array(659, 33), Array(831, 653), Array(923, 635), Array(618, 614), Array(877, 227), Array(118, 68), Array(858, 67), Array(779, 569), Array(884, 203), Array(832, 711), Array(988, 101), Array(673, 96), Array(507, 264), Array(276, 177), Array(498, 281), Array(763, 742), Array(884, 117), Array(865, 312), Array(950, 11), Array(549, 75), Array(718, 626), Array(732, 108), Array(635, 53), Array(687, 529), Array(703, 376), Array(709, 394), Array(710, 595), Array(821, 600), Array(681, 653), Array(957, 265), Array(876, 664), Array(781, 684), Array(141, 111), Array(904, 490), Array(987, 480), Array(698, 134), Array(924, 201), Array(373, 255), Array(863, 320), Array(709, 9), Array(626, 12), Array(737, 512), Array(967, 427), Array(447, 346), Array(785, 240), Array(721, 209), Array(966, 603), Array(735, 49), Array(559, 31), Array(715, 520), Array(413, 64), Array(623, 449), Array(797, 736), Array(385, 93), Array(491, 466), Array(615, 415), Array(875, 168), Array(727, 27), Array(704, 473), Array(518, 106), Array(483, 291), Array(661, 342), Array(793, 248), Array(585, 93), Array(801, 669), Array(368, 303), Array(517, 404), Array(759, 454), Array(291, 147), Array(680, 675), Array(874, 341), Array(690, 288), Array(852, 842), Array(693, 60), Array(455, 396), Array(459, 399), Array(429, 103), Array(697, 210), Array(714, 472), Array(208, 190), Array(932, 929), Array(316, 27), Array(786, 569), Array(719, 502), Array(690, 202), Array(747, 281), Array(603, 573), Array(470, 195), Array(258, 104), Array(188, 74), Array(715, 350), Array(959, 338), Array(696, 81), Array(822, 204), Array(989, 524), Array(654, 339), Array(555, 321), Array(219, 38), Array(971, 327), Array(581, 411), Array(671, 511), Array(658, 46), Array(439, 108), Array(251, 13), Array(255, 102), Array(979, 264), Array(446, 316), Array(748, 243), Array(571, 366), Array(419, 403), Array(345, 50), Array(972, 16), Array(738, 513), Array(509, 359), Array(461, 110), Array(929, 581), Array(811, 585), Array(518, 322), Array(568, 34), Array(900, 49), Array(845, 381), Array(544, 53), Array(964, 576), Array(949, 552), Array(994, 931), Array(715, 462), Array(299, 199), Array(113, 0), Array(921, 518), Array(751, 630), Array(767, 328), Array(812, 106), Array(910, 784), Array(880, 713), Array(773, 720), Array(971, 134), Array(450, 379), Array(704, 146), Array(379, 170), Array(746, 399), Array(790, 113), Array(391, 2), Array(799, 116), Array(845, 193), Array(909, 728), Array(319, 62), Array(620, 172), Array(528, 434), Array(691, 112), Array(621, 588), Array(974, 525), Array(982, 831), Array(120, 111), Array(970, 76), Array(343, 114), Array(633, 64), Array(935, 683), Array(667, 326), Array(843, 399), Array(973, 718), Array(997, 538), Array(725, 282), Array(511, 43), Array(192, 114), Array(967, 856), Array(185, 87), Array(760, 164), Array(435, 18), Array(678, 364), Array(397, 352), Array(500, 47), Array(949, 864), Array(875, 675), Array(699, 583), Array(644, 193), Array(759, 363), Array(732, 367), Array(850, 460), Array(684, 121), Array(778, 131), Array(785, 403), Array(821, 7), Array(536, 275), Array(577, 332), Array(805, 63), Array(582, 75), Array(736, 189), Array(396, 349), Array(726, 473), Array(964, 659), Array(70, 30), Array(894, 206), Array(760, 108), Array(594, 182), Array(648, 452), Array(931, 441), Array(744, 474), Array(971, 868), Array(655, 350), Array(725, 276), Array(932, 596), Array(759, 515), Array(499, 335), Array(941, 535), Array(767, 435), Array(687, 531), Array(348, 325), Array(389, 88), Array(581, 419), Array(742, 665), Array(495, 188), Array(890, 177), Array(927, 674), Array(711, 217), Array(361, 68), Array(487, 407), Array(525, 437), Array(975, 42), Array(599, 122), Array(909, 632), Array(934, 16), Array(880, 165), Array(178, 150), Array(826, 401), Array(433, 119), Array(174, 63), Array(998, 682), Array(664, 571), Array(997, 648), Array(593, 421), Array(58, 53), Array(292, 146), Array(982, 82), Array(734, 682), Array(357, 43), Array(523, 275), Array(330, 282), Array(797, 599), Array(918, 27), Array(478, 50), Array(929, 117), Array(739, 534), Array(268, 148), Array(439, 331), Array(635, 424), Array(901, 465), Array(748, 583), Array(773, 385), Array(742, 598), Array(554, 227), Array(190, 57), Array(523, 10), Array(542, 390), Array(311, 204), Array(952, 72), Array(499, 433), Array(549, 359), Array(932, 502), Array(743, 514), Array(560, 438), Array(395, 223), Array(594, 248), Array(860, 830), Array(291, 179), Array(575, 273), Array(994, 978), Array(721, 325), Array(683, 548), Array(891, 445), Array(204, 135), Array(979, 534), Array(738, 244), Array(656, 572), Array(534, 302), Array(979, 849), Array(932, 829), Array(763, 751), Array(284, 152), Array(137, 104), Array(415, 213), Array(466, 284), Array(180, 21), Array(959, 666), Array(856, 513), Array(960, 888), Array(733, 412), Array(618, 290), Array(589, 463), Array(531, 475), Array(927, 845), Array(849, 179), Array(735, 504), Array(774, 217), Array(992, 580), Array(144, 66), Array(466, 307), Array(446, 324), Array(674, 133), Array(736, 11), Array(726, 373), Array(608, 565), Array(462, 229), Array(726, 351), Array(382, 212), Array(150, 31), Array(317, 298), Array(405, 211), Array(675, 55), Array(563, 342), Array(501, 47), Array(765, 681), Array(937, 129), Array(965, 186), Array(856, 88), Array(837, 326), Array(558, 355), Array(724, 149), Array(311, 104), Array(481, 182), Array(975, 688), Array(757, 640), Array(834, 535), Array(395, 270), Array(311, 5), Array(717, 149), Array(858, 189), Array(457, 146), Array(647, 505), Array(478, 279), Array(832, 308), Array(286, 139), Array(474, 296), Array(608, 482), Array(366, 186), Array(983, 189), Array(949, 256), Array(919, 344), Array(948, 794), Array(720, 251), Array(755, 364), Array(971, 249), Array(840, 512), Array(859, 815), Array(574, 129), Array(758, 281), Array(907, 145), Array(328, 92), Array(909, 583), Array(990, 688), Array(661, 100), Array(777, 188), Array(277, 150), Array(839, 192), Array(532, 507), Array(834, 356), Array(170, 13), Array(514, 211), Array(277, 233), Array(885, 630), Array(371, 245), Array(985, 383), Array(769, 281), Array(886, 135), Array(896, 212), Array(372, 203), Array(767, 312), Array(713, 676), Array(960, 60), Array(521, 50), Array(627, 243), Array(578, 512), Array(575, 326), Array(47, 0), Array(912, 851), Array(439, 192), Array(777, 220), Array(948, 751), Array(265, 235), Array(963, 102), Array(743, 664), Array(514, 218), Array(994, 24), Array(834, 441), Array(790, 300), Array(679, 141), Array(71, 14), Array(965, 87), Array(801, 657), Array(624, 596), Array(288, 86), Array(533, 106), Array(447, 132), Array(881, 223), Array(889, 233), Array(517, 293), Array(488, 89), Array(284, 14), Array(409, 54), Array(542, 221), Array(293, 225), Array(570, 106), Array(902, 641), Array(937, 642), Array(643, 358), Array(467, 73), Array(741, 286), Array(341, 154), Array(768, 138), Array(440, 286), Array(119, 6), Array(967, 451), Array(917, 654), Array(638, 259), Array(982, 76), Array(501, 186), Array(638, 450), Array(558, 164), Array(932, 743), Array(911, 733), Array(292, 74), Array(962, 417), Array(294, 85), Array(136, 103), Array(395, 167), Array(197, 167), Array(985, 682), Array(865, 494), Array(879, 407), Array(708, 419), Array(692, 196), Array(790, 144), Array(634, 500), Array(718, 214), Array(600, 92), Array(320, 213), Array(286, 278), Array(591, 282), Array(720, 565), Array(606, 499), Array(998, 49), Array(992, 316), Array(719, 596), Array(930, 36), Array(847, 583), Array(256, 46), Array(474, 185), Array(583, 114), Array(856, 670), Array(291, 100), Array(487, 235), Array(739, 492), Array(360, 358), Array(625, 239), Array(792, 449), Array(771, 701), Array(867, 598), Array(866, 501), Array(293, 265), Array(863, 115), Array(783, 358), Array(932, 396), Array(524, 282), Array(125, 108), Array(755, 474), Array(894, 566), Array(581, 32), Array(312, 193), Array(799, 670), Array(967, 717), Array(621, 36), Array(800, 428), Array(599, 219), Array(652, 384), Array(607, 136), Array(825, 610), Array(367, 268), Array(533, 7), Array(772, 275), Array(747, 577), Array(887, 124), Array(888, 127), Array(922, 739), Array(909, 744), Array(498, 237), Array(944, 192), Array(653, 28), Array(587, 322), Array(361, 206), Array(555, 313), Array(533, 294), Array(21, 11), Array(381, 340), Array(710, 65), Array(696, 584), Array(783, 558), Array(235, 82), Array(874, 380), Array(656, 125), Array(972, 199), Array(685, 400), Array(947, 450), Array(949, 617), Array(601, 267), Array(828, 278), Array(459, 78), Array(269, 122), Array(235, 75), Array(563, 8), Array(478, 408), Array(803, 8), Array(868, 741), Array(813, 115), Array(789, 370), Array(837, 453), Array(856, 130), Array(501, 356), Array(687, 74), Array(553, 114), Array(282, 174), Array(123, 69), Array(995, 208), Array(823, 241), Array(150, 58), Array(678, 296), Array(755, 524), Array(833, 325), Array(761, 539), Array(807, 600), Array(613, 435), Array(936, 707), Array(622, 132), Array(802, 645), Array(737, 7), Array(998, 142), Array(283, 102), Array(941, 635), Array(351, 287), Array(922, 810), Array(604, 48), Array(573, 55), Array(340, 85), Array(763, 359), Array(773, 44), Array(270, 186), Array(957, 181), Array(956, 764), Array(895, 683), Array(742, 434), Array(832, 30), Array(623, 280), Array(680, 235), Array(420, 298), Array(814, 239), Array(870, 755), Array(838, 464), Array(234, 227), Array(928, 617), Array(966, 4), Array(835, 680), Array(499, 383), Array(683, 507), Array(895, 878), Array(778, 154), Array(629, 596), Array(875, 20), Array(998, 427), Array(985, 318), Array(590, 81), Array(727, 74), Array(430, 212), Array(867, 367), Array(783, 400), Array(978, 526), Array(836, 339), Array(896, 330), Array(993, 896), Array(152, 42), Array(884, 497), Array(745, 134), Array(835, 808), Array(977, 497), Array(477, 464), Array(847, 633), Array(588, 51), Array(546, 384), Array(807, 528), Array(938, 922), Array(800, 75), Array(915, 110), Array(809, 797), Array(682, 622), Array(747, 590), Array(856, 732), Array(376, 45), Array(719, 631), Array(744, 674), Array(238, 0), Array(424, 330), Array(828, 119), Array(963, 754), Array(686, 19), Array(758, 609), Array(666, 366), Array(761, 243), Array(724, 231), Array(817, 801), Array(971, 408), Array(595, 429), Array(684, 67), Array(110, 45), Array(989, 855), Array(632, 178), Array(858, 696), Array(979, 286), Array(723, 51), Array(280, 260), Array(913, 97), Array(760, 601), Array(468, 227), Array(189, 56), Array(996, 187), Array(664, 350), Array(448, 80), Array(290, 204), Array(935, 352), Array(437, 94), Array(893, 498), Array(451, 85), Array(364, 174), Array(271, 198), Array(917, 638), Array(959, 46), Array(498, 239), Array(761, 310), Array(751, 587), Array(959, 284), Array(518, 217), Array(225, 49), Array(667, 133), Array(576, 107), Array(855, 450), Array(395, 393), Array(676, 532), Array(961, 1), Array(533, 210), Array(861, 483), Array(800, 586), Array(461, 52), Array(908, 431), Array(532, 384), Array(697, 188), Array(674, 184), Array(486, 6), Array(815, 22), Array(782, 424), Array(994, 772), Array(406, 194), Array(441, 392), Array(904, 255), Array(998, 867), Array(966, 370), Array(268, 169), Array(347, 153), Array(709, 645), Array(889, 583), Array(248, 102), Array(752, 581), Array(588, 430), Array(189, 102), Array(760, 215), Array(296, 291), Array(931, 87), Array(619, 561), Array(823, 33), Array(652, 535), Array(340, 281), Array(263, 40), Array(821, 471), Array(865, 78), Array(140, 4), Array(930, 258), Array(733, 201), Array(755, 420), Array(579, 198), Array(895, 801), Array(757, 192), Array(511, 215), Array(481, 452), Array(429, 51), Array(919, 313), Array(580, 49))

//  val num = 3
//  val p = Array(Array(0, 1), Array(0, 2), Array(1, 2))
  val res = Solution.canFinish(num, p)
  println(res)

  object Solution {
    def canFinish(numCourses: Int, prerequisites: Array[Array[Int]]): Boolean = {
      lazy val adjList: Array[Set[Int]] = Array.fill(numCourses)(Set())
      var visitedNodes = Set[Int]()
      var nodesBeingVisited = Set[Int]()

      @scala.annotation.tailrec
      def prepAdjList(index: Int): Unit = {
        if (index == prerequisites.length) {
          ()
        } else {
          adjList(prerequisites(index)(1)) = adjList(prerequisites(index)(1)) + prerequisites(index)(0)
          prepAdjList(index + 1)
        }
      }

      @scala.annotation.tailrec
      def check(index: Int): Boolean = {
        if (index == numCourses) {
          true
        } else {
          if (visitedNodes.contains(index)) {
            check(index + 1)
          } else {
            nodesBeingVisited = Set()
            if (hasCycle(index)) {
              false
            } else {
              check(index + 1)
            }
          }
        }
      }

      def hasCycle(node: Int): Boolean = {
        nodesBeingVisited = nodesBeingVisited + node
        val cycleFound =
          adjList(node).find(neighbour => {
            if (visitedNodes.contains(neighbour)) {
              false
            }else if (nodesBeingVisited.contains(neighbour)) {
              true
            } else {
              hasCycle(neighbour)
            }
          }) match {
            case Some(_) => true
            case None => false
          }
        visitedNodes = visitedNodes + node
        cycleFound
      }

      prepAdjList(0)
      println(adjList.mkString(","))
      check(0)
    }
  }

}
