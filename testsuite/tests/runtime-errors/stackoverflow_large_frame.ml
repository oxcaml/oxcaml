(* TEST
   { bytecode; }
   { native; }
*)

(* Deep recursion where every frame is several pages. Without stack
   checks (amd64), functions with such frames must keep an explicit
   check: an overflowing access could leap the stack's guard page
   (crashing, or silently corrupting whatever lies beyond) instead of
   faulting in it. The [x0]...[x999] bindings are live across the
   recursive call, forcing a spill slot each (an ~8KiB frame). *)

let o = Sys.opaque_identity

let rec f d =
  if d = 0
  then 0.0
  else begin
    let x0 = o 0. in let x1 = o 1. in let x2 = o 2. in
    let x3 = o 3. in let x4 = o 4. in let x5 = o 5. in
    let x6 = o 6. in let x7 = o 7. in let x8 = o 8. in
    let x9 = o 9. in let x10 = o 10. in let x11 = o 11. in
    let x12 = o 12. in let x13 = o 13. in let x14 = o 14. in
    let x15 = o 15. in let x16 = o 16. in let x17 = o 17. in
    let x18 = o 18. in let x19 = o 19. in let x20 = o 20. in
    let x21 = o 21. in let x22 = o 22. in let x23 = o 23. in
    let x24 = o 24. in let x25 = o 25. in let x26 = o 26. in
    let x27 = o 27. in let x28 = o 28. in let x29 = o 29. in
    let x30 = o 30. in let x31 = o 31. in let x32 = o 32. in
    let x33 = o 33. in let x34 = o 34. in let x35 = o 35. in
    let x36 = o 36. in let x37 = o 37. in let x38 = o 38. in
    let x39 = o 39. in let x40 = o 40. in let x41 = o 41. in
    let x42 = o 42. in let x43 = o 43. in let x44 = o 44. in
    let x45 = o 45. in let x46 = o 46. in let x47 = o 47. in
    let x48 = o 48. in let x49 = o 49. in let x50 = o 50. in
    let x51 = o 51. in let x52 = o 52. in let x53 = o 53. in
    let x54 = o 54. in let x55 = o 55. in let x56 = o 56. in
    let x57 = o 57. in let x58 = o 58. in let x59 = o 59. in
    let x60 = o 60. in let x61 = o 61. in let x62 = o 62. in
    let x63 = o 63. in let x64 = o 64. in let x65 = o 65. in
    let x66 = o 66. in let x67 = o 67. in let x68 = o 68. in
    let x69 = o 69. in let x70 = o 70. in let x71 = o 71. in
    let x72 = o 72. in let x73 = o 73. in let x74 = o 74. in
    let x75 = o 75. in let x76 = o 76. in let x77 = o 77. in
    let x78 = o 78. in let x79 = o 79. in let x80 = o 80. in
    let x81 = o 81. in let x82 = o 82. in let x83 = o 83. in
    let x84 = o 84. in let x85 = o 85. in let x86 = o 86. in
    let x87 = o 87. in let x88 = o 88. in let x89 = o 89. in
    let x90 = o 90. in let x91 = o 91. in let x92 = o 92. in
    let x93 = o 93. in let x94 = o 94. in let x95 = o 95. in
    let x96 = o 96. in let x97 = o 97. in let x98 = o 98. in
    let x99 = o 99. in let x100 = o 100. in let x101 = o 101. in
    let x102 = o 102. in let x103 = o 103. in let x104 = o 104. in
    let x105 = o 105. in let x106 = o 106. in let x107 = o 107. in
    let x108 = o 108. in let x109 = o 109. in let x110 = o 110. in
    let x111 = o 111. in let x112 = o 112. in let x113 = o 113. in
    let x114 = o 114. in let x115 = o 115. in let x116 = o 116. in
    let x117 = o 117. in let x118 = o 118. in let x119 = o 119. in
    let x120 = o 120. in let x121 = o 121. in let x122 = o 122. in
    let x123 = o 123. in let x124 = o 124. in let x125 = o 125. in
    let x126 = o 126. in let x127 = o 127. in let x128 = o 128. in
    let x129 = o 129. in let x130 = o 130. in let x131 = o 131. in
    let x132 = o 132. in let x133 = o 133. in let x134 = o 134. in
    let x135 = o 135. in let x136 = o 136. in let x137 = o 137. in
    let x138 = o 138. in let x139 = o 139. in let x140 = o 140. in
    let x141 = o 141. in let x142 = o 142. in let x143 = o 143. in
    let x144 = o 144. in let x145 = o 145. in let x146 = o 146. in
    let x147 = o 147. in let x148 = o 148. in let x149 = o 149. in
    let x150 = o 150. in let x151 = o 151. in let x152 = o 152. in
    let x153 = o 153. in let x154 = o 154. in let x155 = o 155. in
    let x156 = o 156. in let x157 = o 157. in let x158 = o 158. in
    let x159 = o 159. in let x160 = o 160. in let x161 = o 161. in
    let x162 = o 162. in let x163 = o 163. in let x164 = o 164. in
    let x165 = o 165. in let x166 = o 166. in let x167 = o 167. in
    let x168 = o 168. in let x169 = o 169. in let x170 = o 170. in
    let x171 = o 171. in let x172 = o 172. in let x173 = o 173. in
    let x174 = o 174. in let x175 = o 175. in let x176 = o 176. in
    let x177 = o 177. in let x178 = o 178. in let x179 = o 179. in
    let x180 = o 180. in let x181 = o 181. in let x182 = o 182. in
    let x183 = o 183. in let x184 = o 184. in let x185 = o 185. in
    let x186 = o 186. in let x187 = o 187. in let x188 = o 188. in
    let x189 = o 189. in let x190 = o 190. in let x191 = o 191. in
    let x192 = o 192. in let x193 = o 193. in let x194 = o 194. in
    let x195 = o 195. in let x196 = o 196. in let x197 = o 197. in
    let x198 = o 198. in let x199 = o 199. in let x200 = o 200. in
    let x201 = o 201. in let x202 = o 202. in let x203 = o 203. in
    let x204 = o 204. in let x205 = o 205. in let x206 = o 206. in
    let x207 = o 207. in let x208 = o 208. in let x209 = o 209. in
    let x210 = o 210. in let x211 = o 211. in let x212 = o 212. in
    let x213 = o 213. in let x214 = o 214. in let x215 = o 215. in
    let x216 = o 216. in let x217 = o 217. in let x218 = o 218. in
    let x219 = o 219. in let x220 = o 220. in let x221 = o 221. in
    let x222 = o 222. in let x223 = o 223. in let x224 = o 224. in
    let x225 = o 225. in let x226 = o 226. in let x227 = o 227. in
    let x228 = o 228. in let x229 = o 229. in let x230 = o 230. in
    let x231 = o 231. in let x232 = o 232. in let x233 = o 233. in
    let x234 = o 234. in let x235 = o 235. in let x236 = o 236. in
    let x237 = o 237. in let x238 = o 238. in let x239 = o 239. in
    let x240 = o 240. in let x241 = o 241. in let x242 = o 242. in
    let x243 = o 243. in let x244 = o 244. in let x245 = o 245. in
    let x246 = o 246. in let x247 = o 247. in let x248 = o 248. in
    let x249 = o 249. in let x250 = o 250. in let x251 = o 251. in
    let x252 = o 252. in let x253 = o 253. in let x254 = o 254. in
    let x255 = o 255. in let x256 = o 256. in let x257 = o 257. in
    let x258 = o 258. in let x259 = o 259. in let x260 = o 260. in
    let x261 = o 261. in let x262 = o 262. in let x263 = o 263. in
    let x264 = o 264. in let x265 = o 265. in let x266 = o 266. in
    let x267 = o 267. in let x268 = o 268. in let x269 = o 269. in
    let x270 = o 270. in let x271 = o 271. in let x272 = o 272. in
    let x273 = o 273. in let x274 = o 274. in let x275 = o 275. in
    let x276 = o 276. in let x277 = o 277. in let x278 = o 278. in
    let x279 = o 279. in let x280 = o 280. in let x281 = o 281. in
    let x282 = o 282. in let x283 = o 283. in let x284 = o 284. in
    let x285 = o 285. in let x286 = o 286. in let x287 = o 287. in
    let x288 = o 288. in let x289 = o 289. in let x290 = o 290. in
    let x291 = o 291. in let x292 = o 292. in let x293 = o 293. in
    let x294 = o 294. in let x295 = o 295. in let x296 = o 296. in
    let x297 = o 297. in let x298 = o 298. in let x299 = o 299. in
    let x300 = o 300. in let x301 = o 301. in let x302 = o 302. in
    let x303 = o 303. in let x304 = o 304. in let x305 = o 305. in
    let x306 = o 306. in let x307 = o 307. in let x308 = o 308. in
    let x309 = o 309. in let x310 = o 310. in let x311 = o 311. in
    let x312 = o 312. in let x313 = o 313. in let x314 = o 314. in
    let x315 = o 315. in let x316 = o 316. in let x317 = o 317. in
    let x318 = o 318. in let x319 = o 319. in let x320 = o 320. in
    let x321 = o 321. in let x322 = o 322. in let x323 = o 323. in
    let x324 = o 324. in let x325 = o 325. in let x326 = o 326. in
    let x327 = o 327. in let x328 = o 328. in let x329 = o 329. in
    let x330 = o 330. in let x331 = o 331. in let x332 = o 332. in
    let x333 = o 333. in let x334 = o 334. in let x335 = o 335. in
    let x336 = o 336. in let x337 = o 337. in let x338 = o 338. in
    let x339 = o 339. in let x340 = o 340. in let x341 = o 341. in
    let x342 = o 342. in let x343 = o 343. in let x344 = o 344. in
    let x345 = o 345. in let x346 = o 346. in let x347 = o 347. in
    let x348 = o 348. in let x349 = o 349. in let x350 = o 350. in
    let x351 = o 351. in let x352 = o 352. in let x353 = o 353. in
    let x354 = o 354. in let x355 = o 355. in let x356 = o 356. in
    let x357 = o 357. in let x358 = o 358. in let x359 = o 359. in
    let x360 = o 360. in let x361 = o 361. in let x362 = o 362. in
    let x363 = o 363. in let x364 = o 364. in let x365 = o 365. in
    let x366 = o 366. in let x367 = o 367. in let x368 = o 368. in
    let x369 = o 369. in let x370 = o 370. in let x371 = o 371. in
    let x372 = o 372. in let x373 = o 373. in let x374 = o 374. in
    let x375 = o 375. in let x376 = o 376. in let x377 = o 377. in
    let x378 = o 378. in let x379 = o 379. in let x380 = o 380. in
    let x381 = o 381. in let x382 = o 382. in let x383 = o 383. in
    let x384 = o 384. in let x385 = o 385. in let x386 = o 386. in
    let x387 = o 387. in let x388 = o 388. in let x389 = o 389. in
    let x390 = o 390. in let x391 = o 391. in let x392 = o 392. in
    let x393 = o 393. in let x394 = o 394. in let x395 = o 395. in
    let x396 = o 396. in let x397 = o 397. in let x398 = o 398. in
    let x399 = o 399. in let x400 = o 400. in let x401 = o 401. in
    let x402 = o 402. in let x403 = o 403. in let x404 = o 404. in
    let x405 = o 405. in let x406 = o 406. in let x407 = o 407. in
    let x408 = o 408. in let x409 = o 409. in let x410 = o 410. in
    let x411 = o 411. in let x412 = o 412. in let x413 = o 413. in
    let x414 = o 414. in let x415 = o 415. in let x416 = o 416. in
    let x417 = o 417. in let x418 = o 418. in let x419 = o 419. in
    let x420 = o 420. in let x421 = o 421. in let x422 = o 422. in
    let x423 = o 423. in let x424 = o 424. in let x425 = o 425. in
    let x426 = o 426. in let x427 = o 427. in let x428 = o 428. in
    let x429 = o 429. in let x430 = o 430. in let x431 = o 431. in
    let x432 = o 432. in let x433 = o 433. in let x434 = o 434. in
    let x435 = o 435. in let x436 = o 436. in let x437 = o 437. in
    let x438 = o 438. in let x439 = o 439. in let x440 = o 440. in
    let x441 = o 441. in let x442 = o 442. in let x443 = o 443. in
    let x444 = o 444. in let x445 = o 445. in let x446 = o 446. in
    let x447 = o 447. in let x448 = o 448. in let x449 = o 449. in
    let x450 = o 450. in let x451 = o 451. in let x452 = o 452. in
    let x453 = o 453. in let x454 = o 454. in let x455 = o 455. in
    let x456 = o 456. in let x457 = o 457. in let x458 = o 458. in
    let x459 = o 459. in let x460 = o 460. in let x461 = o 461. in
    let x462 = o 462. in let x463 = o 463. in let x464 = o 464. in
    let x465 = o 465. in let x466 = o 466. in let x467 = o 467. in
    let x468 = o 468. in let x469 = o 469. in let x470 = o 470. in
    let x471 = o 471. in let x472 = o 472. in let x473 = o 473. in
    let x474 = o 474. in let x475 = o 475. in let x476 = o 476. in
    let x477 = o 477. in let x478 = o 478. in let x479 = o 479. in
    let x480 = o 480. in let x481 = o 481. in let x482 = o 482. in
    let x483 = o 483. in let x484 = o 484. in let x485 = o 485. in
    let x486 = o 486. in let x487 = o 487. in let x488 = o 488. in
    let x489 = o 489. in let x490 = o 490. in let x491 = o 491. in
    let x492 = o 492. in let x493 = o 493. in let x494 = o 494. in
    let x495 = o 495. in let x496 = o 496. in let x497 = o 497. in
    let x498 = o 498. in let x499 = o 499. in let x500 = o 500. in
    let x501 = o 501. in let x502 = o 502. in let x503 = o 503. in
    let x504 = o 504. in let x505 = o 505. in let x506 = o 506. in
    let x507 = o 507. in let x508 = o 508. in let x509 = o 509. in
    let x510 = o 510. in let x511 = o 511. in let x512 = o 512. in
    let x513 = o 513. in let x514 = o 514. in let x515 = o 515. in
    let x516 = o 516. in let x517 = o 517. in let x518 = o 518. in
    let x519 = o 519. in let x520 = o 520. in let x521 = o 521. in
    let x522 = o 522. in let x523 = o 523. in let x524 = o 524. in
    let x525 = o 525. in let x526 = o 526. in let x527 = o 527. in
    let x528 = o 528. in let x529 = o 529. in let x530 = o 530. in
    let x531 = o 531. in let x532 = o 532. in let x533 = o 533. in
    let x534 = o 534. in let x535 = o 535. in let x536 = o 536. in
    let x537 = o 537. in let x538 = o 538. in let x539 = o 539. in
    let x540 = o 540. in let x541 = o 541. in let x542 = o 542. in
    let x543 = o 543. in let x544 = o 544. in let x545 = o 545. in
    let x546 = o 546. in let x547 = o 547. in let x548 = o 548. in
    let x549 = o 549. in let x550 = o 550. in let x551 = o 551. in
    let x552 = o 552. in let x553 = o 553. in let x554 = o 554. in
    let x555 = o 555. in let x556 = o 556. in let x557 = o 557. in
    let x558 = o 558. in let x559 = o 559. in let x560 = o 560. in
    let x561 = o 561. in let x562 = o 562. in let x563 = o 563. in
    let x564 = o 564. in let x565 = o 565. in let x566 = o 566. in
    let x567 = o 567. in let x568 = o 568. in let x569 = o 569. in
    let x570 = o 570. in let x571 = o 571. in let x572 = o 572. in
    let x573 = o 573. in let x574 = o 574. in let x575 = o 575. in
    let x576 = o 576. in let x577 = o 577. in let x578 = o 578. in
    let x579 = o 579. in let x580 = o 580. in let x581 = o 581. in
    let x582 = o 582. in let x583 = o 583. in let x584 = o 584. in
    let x585 = o 585. in let x586 = o 586. in let x587 = o 587. in
    let x588 = o 588. in let x589 = o 589. in let x590 = o 590. in
    let x591 = o 591. in let x592 = o 592. in let x593 = o 593. in
    let x594 = o 594. in let x595 = o 595. in let x596 = o 596. in
    let x597 = o 597. in let x598 = o 598. in let x599 = o 599. in
    let x600 = o 600. in let x601 = o 601. in let x602 = o 602. in
    let x603 = o 603. in let x604 = o 604. in let x605 = o 605. in
    let x606 = o 606. in let x607 = o 607. in let x608 = o 608. in
    let x609 = o 609. in let x610 = o 610. in let x611 = o 611. in
    let x612 = o 612. in let x613 = o 613. in let x614 = o 614. in
    let x615 = o 615. in let x616 = o 616. in let x617 = o 617. in
    let x618 = o 618. in let x619 = o 619. in let x620 = o 620. in
    let x621 = o 621. in let x622 = o 622. in let x623 = o 623. in
    let x624 = o 624. in let x625 = o 625. in let x626 = o 626. in
    let x627 = o 627. in let x628 = o 628. in let x629 = o 629. in
    let x630 = o 630. in let x631 = o 631. in let x632 = o 632. in
    let x633 = o 633. in let x634 = o 634. in let x635 = o 635. in
    let x636 = o 636. in let x637 = o 637. in let x638 = o 638. in
    let x639 = o 639. in let x640 = o 640. in let x641 = o 641. in
    let x642 = o 642. in let x643 = o 643. in let x644 = o 644. in
    let x645 = o 645. in let x646 = o 646. in let x647 = o 647. in
    let x648 = o 648. in let x649 = o 649. in let x650 = o 650. in
    let x651 = o 651. in let x652 = o 652. in let x653 = o 653. in
    let x654 = o 654. in let x655 = o 655. in let x656 = o 656. in
    let x657 = o 657. in let x658 = o 658. in let x659 = o 659. in
    let x660 = o 660. in let x661 = o 661. in let x662 = o 662. in
    let x663 = o 663. in let x664 = o 664. in let x665 = o 665. in
    let x666 = o 666. in let x667 = o 667. in let x668 = o 668. in
    let x669 = o 669. in let x670 = o 670. in let x671 = o 671. in
    let x672 = o 672. in let x673 = o 673. in let x674 = o 674. in
    let x675 = o 675. in let x676 = o 676. in let x677 = o 677. in
    let x678 = o 678. in let x679 = o 679. in let x680 = o 680. in
    let x681 = o 681. in let x682 = o 682. in let x683 = o 683. in
    let x684 = o 684. in let x685 = o 685. in let x686 = o 686. in
    let x687 = o 687. in let x688 = o 688. in let x689 = o 689. in
    let x690 = o 690. in let x691 = o 691. in let x692 = o 692. in
    let x693 = o 693. in let x694 = o 694. in let x695 = o 695. in
    let x696 = o 696. in let x697 = o 697. in let x698 = o 698. in
    let x699 = o 699. in let x700 = o 700. in let x701 = o 701. in
    let x702 = o 702. in let x703 = o 703. in let x704 = o 704. in
    let x705 = o 705. in let x706 = o 706. in let x707 = o 707. in
    let x708 = o 708. in let x709 = o 709. in let x710 = o 710. in
    let x711 = o 711. in let x712 = o 712. in let x713 = o 713. in
    let x714 = o 714. in let x715 = o 715. in let x716 = o 716. in
    let x717 = o 717. in let x718 = o 718. in let x719 = o 719. in
    let x720 = o 720. in let x721 = o 721. in let x722 = o 722. in
    let x723 = o 723. in let x724 = o 724. in let x725 = o 725. in
    let x726 = o 726. in let x727 = o 727. in let x728 = o 728. in
    let x729 = o 729. in let x730 = o 730. in let x731 = o 731. in
    let x732 = o 732. in let x733 = o 733. in let x734 = o 734. in
    let x735 = o 735. in let x736 = o 736. in let x737 = o 737. in
    let x738 = o 738. in let x739 = o 739. in let x740 = o 740. in
    let x741 = o 741. in let x742 = o 742. in let x743 = o 743. in
    let x744 = o 744. in let x745 = o 745. in let x746 = o 746. in
    let x747 = o 747. in let x748 = o 748. in let x749 = o 749. in
    let x750 = o 750. in let x751 = o 751. in let x752 = o 752. in
    let x753 = o 753. in let x754 = o 754. in let x755 = o 755. in
    let x756 = o 756. in let x757 = o 757. in let x758 = o 758. in
    let x759 = o 759. in let x760 = o 760. in let x761 = o 761. in
    let x762 = o 762. in let x763 = o 763. in let x764 = o 764. in
    let x765 = o 765. in let x766 = o 766. in let x767 = o 767. in
    let x768 = o 768. in let x769 = o 769. in let x770 = o 770. in
    let x771 = o 771. in let x772 = o 772. in let x773 = o 773. in
    let x774 = o 774. in let x775 = o 775. in let x776 = o 776. in
    let x777 = o 777. in let x778 = o 778. in let x779 = o 779. in
    let x780 = o 780. in let x781 = o 781. in let x782 = o 782. in
    let x783 = o 783. in let x784 = o 784. in let x785 = o 785. in
    let x786 = o 786. in let x787 = o 787. in let x788 = o 788. in
    let x789 = o 789. in let x790 = o 790. in let x791 = o 791. in
    let x792 = o 792. in let x793 = o 793. in let x794 = o 794. in
    let x795 = o 795. in let x796 = o 796. in let x797 = o 797. in
    let x798 = o 798. in let x799 = o 799. in let x800 = o 800. in
    let x801 = o 801. in let x802 = o 802. in let x803 = o 803. in
    let x804 = o 804. in let x805 = o 805. in let x806 = o 806. in
    let x807 = o 807. in let x808 = o 808. in let x809 = o 809. in
    let x810 = o 810. in let x811 = o 811. in let x812 = o 812. in
    let x813 = o 813. in let x814 = o 814. in let x815 = o 815. in
    let x816 = o 816. in let x817 = o 817. in let x818 = o 818. in
    let x819 = o 819. in let x820 = o 820. in let x821 = o 821. in
    let x822 = o 822. in let x823 = o 823. in let x824 = o 824. in
    let x825 = o 825. in let x826 = o 826. in let x827 = o 827. in
    let x828 = o 828. in let x829 = o 829. in let x830 = o 830. in
    let x831 = o 831. in let x832 = o 832. in let x833 = o 833. in
    let x834 = o 834. in let x835 = o 835. in let x836 = o 836. in
    let x837 = o 837. in let x838 = o 838. in let x839 = o 839. in
    let x840 = o 840. in let x841 = o 841. in let x842 = o 842. in
    let x843 = o 843. in let x844 = o 844. in let x845 = o 845. in
    let x846 = o 846. in let x847 = o 847. in let x848 = o 848. in
    let x849 = o 849. in let x850 = o 850. in let x851 = o 851. in
    let x852 = o 852. in let x853 = o 853. in let x854 = o 854. in
    let x855 = o 855. in let x856 = o 856. in let x857 = o 857. in
    let x858 = o 858. in let x859 = o 859. in let x860 = o 860. in
    let x861 = o 861. in let x862 = o 862. in let x863 = o 863. in
    let x864 = o 864. in let x865 = o 865. in let x866 = o 866. in
    let x867 = o 867. in let x868 = o 868. in let x869 = o 869. in
    let x870 = o 870. in let x871 = o 871. in let x872 = o 872. in
    let x873 = o 873. in let x874 = o 874. in let x875 = o 875. in
    let x876 = o 876. in let x877 = o 877. in let x878 = o 878. in
    let x879 = o 879. in let x880 = o 880. in let x881 = o 881. in
    let x882 = o 882. in let x883 = o 883. in let x884 = o 884. in
    let x885 = o 885. in let x886 = o 886. in let x887 = o 887. in
    let x888 = o 888. in let x889 = o 889. in let x890 = o 890. in
    let x891 = o 891. in let x892 = o 892. in let x893 = o 893. in
    let x894 = o 894. in let x895 = o 895. in let x896 = o 896. in
    let x897 = o 897. in let x898 = o 898. in let x899 = o 899. in
    let x900 = o 900. in let x901 = o 901. in let x902 = o 902. in
    let x903 = o 903. in let x904 = o 904. in let x905 = o 905. in
    let x906 = o 906. in let x907 = o 907. in let x908 = o 908. in
    let x909 = o 909. in let x910 = o 910. in let x911 = o 911. in
    let x912 = o 912. in let x913 = o 913. in let x914 = o 914. in
    let x915 = o 915. in let x916 = o 916. in let x917 = o 917. in
    let x918 = o 918. in let x919 = o 919. in let x920 = o 920. in
    let x921 = o 921. in let x922 = o 922. in let x923 = o 923. in
    let x924 = o 924. in let x925 = o 925. in let x926 = o 926. in
    let x927 = o 927. in let x928 = o 928. in let x929 = o 929. in
    let x930 = o 930. in let x931 = o 931. in let x932 = o 932. in
    let x933 = o 933. in let x934 = o 934. in let x935 = o 935. in
    let x936 = o 936. in let x937 = o 937. in let x938 = o 938. in
    let x939 = o 939. in let x940 = o 940. in let x941 = o 941. in
    let x942 = o 942. in let x943 = o 943. in let x944 = o 944. in
    let x945 = o 945. in let x946 = o 946. in let x947 = o 947. in
    let x948 = o 948. in let x949 = o 949. in let x950 = o 950. in
    let x951 = o 951. in let x952 = o 952. in let x953 = o 953. in
    let x954 = o 954. in let x955 = o 955. in let x956 = o 956. in
    let x957 = o 957. in let x958 = o 958. in let x959 = o 959. in
    let x960 = o 960. in let x961 = o 961. in let x962 = o 962. in
    let x963 = o 963. in let x964 = o 964. in let x965 = o 965. in
    let x966 = o 966. in let x967 = o 967. in let x968 = o 968. in
    let x969 = o 969. in let x970 = o 970. in let x971 = o 971. in
    let x972 = o 972. in let x973 = o 973. in let x974 = o 974. in
    let x975 = o 975. in let x976 = o 976. in let x977 = o 977. in
    let x978 = o 978. in let x979 = o 979. in let x980 = o 980. in
    let x981 = o 981. in let x982 = o 982. in let x983 = o 983. in
    let x984 = o 984. in let x985 = o 985. in let x986 = o 986. in
    let x987 = o 987. in let x988 = o 988. in let x989 = o 989. in
    let x990 = o 990. in let x991 = o 991. in let x992 = o 992. in
    let x993 = o 993. in let x994 = o 994. in let x995 = o 995. in
    let x996 = o 996. in let x997 = o 997. in let x998 = o 998. in
    let x999 = o 999. in
    let r = f (d - 1) in
    r
    +. x0 +. x1 +. x2 +. x3 +. x4 +. x5 +. x6 +. x7
    +. x8 +. x9 +. x10 +. x11 +. x12 +. x13 +. x14 +. x15
    +. x16 +. x17 +. x18 +. x19 +. x20 +. x21 +. x22 +. x23
    +. x24 +. x25 +. x26 +. x27 +. x28 +. x29 +. x30 +. x31
    +. x32 +. x33 +. x34 +. x35 +. x36 +. x37 +. x38 +. x39
    +. x40 +. x41 +. x42 +. x43 +. x44 +. x45 +. x46 +. x47
    +. x48 +. x49 +. x50 +. x51 +. x52 +. x53 +. x54 +. x55
    +. x56 +. x57 +. x58 +. x59 +. x60 +. x61 +. x62 +. x63
    +. x64 +. x65 +. x66 +. x67 +. x68 +. x69 +. x70 +. x71
    +. x72 +. x73 +. x74 +. x75 +. x76 +. x77 +. x78 +. x79
    +. x80 +. x81 +. x82 +. x83 +. x84 +. x85 +. x86 +. x87
    +. x88 +. x89 +. x90 +. x91 +. x92 +. x93 +. x94 +. x95
    +. x96 +. x97 +. x98 +. x99 +. x100 +. x101 +. x102 +. x103
    +. x104 +. x105 +. x106 +. x107 +. x108 +. x109 +. x110 +. x111
    +. x112 +. x113 +. x114 +. x115 +. x116 +. x117 +. x118 +. x119
    +. x120 +. x121 +. x122 +. x123 +. x124 +. x125 +. x126 +. x127
    +. x128 +. x129 +. x130 +. x131 +. x132 +. x133 +. x134 +. x135
    +. x136 +. x137 +. x138 +. x139 +. x140 +. x141 +. x142 +. x143
    +. x144 +. x145 +. x146 +. x147 +. x148 +. x149 +. x150 +. x151
    +. x152 +. x153 +. x154 +. x155 +. x156 +. x157 +. x158 +. x159
    +. x160 +. x161 +. x162 +. x163 +. x164 +. x165 +. x166 +. x167
    +. x168 +. x169 +. x170 +. x171 +. x172 +. x173 +. x174 +. x175
    +. x176 +. x177 +. x178 +. x179 +. x180 +. x181 +. x182 +. x183
    +. x184 +. x185 +. x186 +. x187 +. x188 +. x189 +. x190 +. x191
    +. x192 +. x193 +. x194 +. x195 +. x196 +. x197 +. x198 +. x199
    +. x200 +. x201 +. x202 +. x203 +. x204 +. x205 +. x206 +. x207
    +. x208 +. x209 +. x210 +. x211 +. x212 +. x213 +. x214 +. x215
    +. x216 +. x217 +. x218 +. x219 +. x220 +. x221 +. x222 +. x223
    +. x224 +. x225 +. x226 +. x227 +. x228 +. x229 +. x230 +. x231
    +. x232 +. x233 +. x234 +. x235 +. x236 +. x237 +. x238 +. x239
    +. x240 +. x241 +. x242 +. x243 +. x244 +. x245 +. x246 +. x247
    +. x248 +. x249 +. x250 +. x251 +. x252 +. x253 +. x254 +. x255
    +. x256 +. x257 +. x258 +. x259 +. x260 +. x261 +. x262 +. x263
    +. x264 +. x265 +. x266 +. x267 +. x268 +. x269 +. x270 +. x271
    +. x272 +. x273 +. x274 +. x275 +. x276 +. x277 +. x278 +. x279
    +. x280 +. x281 +. x282 +. x283 +. x284 +. x285 +. x286 +. x287
    +. x288 +. x289 +. x290 +. x291 +. x292 +. x293 +. x294 +. x295
    +. x296 +. x297 +. x298 +. x299 +. x300 +. x301 +. x302 +. x303
    +. x304 +. x305 +. x306 +. x307 +. x308 +. x309 +. x310 +. x311
    +. x312 +. x313 +. x314 +. x315 +. x316 +. x317 +. x318 +. x319
    +. x320 +. x321 +. x322 +. x323 +. x324 +. x325 +. x326 +. x327
    +. x328 +. x329 +. x330 +. x331 +. x332 +. x333 +. x334 +. x335
    +. x336 +. x337 +. x338 +. x339 +. x340 +. x341 +. x342 +. x343
    +. x344 +. x345 +. x346 +. x347 +. x348 +. x349 +. x350 +. x351
    +. x352 +. x353 +. x354 +. x355 +. x356 +. x357 +. x358 +. x359
    +. x360 +. x361 +. x362 +. x363 +. x364 +. x365 +. x366 +. x367
    +. x368 +. x369 +. x370 +. x371 +. x372 +. x373 +. x374 +. x375
    +. x376 +. x377 +. x378 +. x379 +. x380 +. x381 +. x382 +. x383
    +. x384 +. x385 +. x386 +. x387 +. x388 +. x389 +. x390 +. x391
    +. x392 +. x393 +. x394 +. x395 +. x396 +. x397 +. x398 +. x399
    +. x400 +. x401 +. x402 +. x403 +. x404 +. x405 +. x406 +. x407
    +. x408 +. x409 +. x410 +. x411 +. x412 +. x413 +. x414 +. x415
    +. x416 +. x417 +. x418 +. x419 +. x420 +. x421 +. x422 +. x423
    +. x424 +. x425 +. x426 +. x427 +. x428 +. x429 +. x430 +. x431
    +. x432 +. x433 +. x434 +. x435 +. x436 +. x437 +. x438 +. x439
    +. x440 +. x441 +. x442 +. x443 +. x444 +. x445 +. x446 +. x447
    +. x448 +. x449 +. x450 +. x451 +. x452 +. x453 +. x454 +. x455
    +. x456 +. x457 +. x458 +. x459 +. x460 +. x461 +. x462 +. x463
    +. x464 +. x465 +. x466 +. x467 +. x468 +. x469 +. x470 +. x471
    +. x472 +. x473 +. x474 +. x475 +. x476 +. x477 +. x478 +. x479
    +. x480 +. x481 +. x482 +. x483 +. x484 +. x485 +. x486 +. x487
    +. x488 +. x489 +. x490 +. x491 +. x492 +. x493 +. x494 +. x495
    +. x496 +. x497 +. x498 +. x499 +. x500 +. x501 +. x502 +. x503
    +. x504 +. x505 +. x506 +. x507 +. x508 +. x509 +. x510 +. x511
    +. x512 +. x513 +. x514 +. x515 +. x516 +. x517 +. x518 +. x519
    +. x520 +. x521 +. x522 +. x523 +. x524 +. x525 +. x526 +. x527
    +. x528 +. x529 +. x530 +. x531 +. x532 +. x533 +. x534 +. x535
    +. x536 +. x537 +. x538 +. x539 +. x540 +. x541 +. x542 +. x543
    +. x544 +. x545 +. x546 +. x547 +. x548 +. x549 +. x550 +. x551
    +. x552 +. x553 +. x554 +. x555 +. x556 +. x557 +. x558 +. x559
    +. x560 +. x561 +. x562 +. x563 +. x564 +. x565 +. x566 +. x567
    +. x568 +. x569 +. x570 +. x571 +. x572 +. x573 +. x574 +. x575
    +. x576 +. x577 +. x578 +. x579 +. x580 +. x581 +. x582 +. x583
    +. x584 +. x585 +. x586 +. x587 +. x588 +. x589 +. x590 +. x591
    +. x592 +. x593 +. x594 +. x595 +. x596 +. x597 +. x598 +. x599
    +. x600 +. x601 +. x602 +. x603 +. x604 +. x605 +. x606 +. x607
    +. x608 +. x609 +. x610 +. x611 +. x612 +. x613 +. x614 +. x615
    +. x616 +. x617 +. x618 +. x619 +. x620 +. x621 +. x622 +. x623
    +. x624 +. x625 +. x626 +. x627 +. x628 +. x629 +. x630 +. x631
    +. x632 +. x633 +. x634 +. x635 +. x636 +. x637 +. x638 +. x639
    +. x640 +. x641 +. x642 +. x643 +. x644 +. x645 +. x646 +. x647
    +. x648 +. x649 +. x650 +. x651 +. x652 +. x653 +. x654 +. x655
    +. x656 +. x657 +. x658 +. x659 +. x660 +. x661 +. x662 +. x663
    +. x664 +. x665 +. x666 +. x667 +. x668 +. x669 +. x670 +. x671
    +. x672 +. x673 +. x674 +. x675 +. x676 +. x677 +. x678 +. x679
    +. x680 +. x681 +. x682 +. x683 +. x684 +. x685 +. x686 +. x687
    +. x688 +. x689 +. x690 +. x691 +. x692 +. x693 +. x694 +. x695
    +. x696 +. x697 +. x698 +. x699 +. x700 +. x701 +. x702 +. x703
    +. x704 +. x705 +. x706 +. x707 +. x708 +. x709 +. x710 +. x711
    +. x712 +. x713 +. x714 +. x715 +. x716 +. x717 +. x718 +. x719
    +. x720 +. x721 +. x722 +. x723 +. x724 +. x725 +. x726 +. x727
    +. x728 +. x729 +. x730 +. x731 +. x732 +. x733 +. x734 +. x735
    +. x736 +. x737 +. x738 +. x739 +. x740 +. x741 +. x742 +. x743
    +. x744 +. x745 +. x746 +. x747 +. x748 +. x749 +. x750 +. x751
    +. x752 +. x753 +. x754 +. x755 +. x756 +. x757 +. x758 +. x759
    +. x760 +. x761 +. x762 +. x763 +. x764 +. x765 +. x766 +. x767
    +. x768 +. x769 +. x770 +. x771 +. x772 +. x773 +. x774 +. x775
    +. x776 +. x777 +. x778 +. x779 +. x780 +. x781 +. x782 +. x783
    +. x784 +. x785 +. x786 +. x787 +. x788 +. x789 +. x790 +. x791
    +. x792 +. x793 +. x794 +. x795 +. x796 +. x797 +. x798 +. x799
    +. x800 +. x801 +. x802 +. x803 +. x804 +. x805 +. x806 +. x807
    +. x808 +. x809 +. x810 +. x811 +. x812 +. x813 +. x814 +. x815
    +. x816 +. x817 +. x818 +. x819 +. x820 +. x821 +. x822 +. x823
    +. x824 +. x825 +. x826 +. x827 +. x828 +. x829 +. x830 +. x831
    +. x832 +. x833 +. x834 +. x835 +. x836 +. x837 +. x838 +. x839
    +. x840 +. x841 +. x842 +. x843 +. x844 +. x845 +. x846 +. x847
    +. x848 +. x849 +. x850 +. x851 +. x852 +. x853 +. x854 +. x855
    +. x856 +. x857 +. x858 +. x859 +. x860 +. x861 +. x862 +. x863
    +. x864 +. x865 +. x866 +. x867 +. x868 +. x869 +. x870 +. x871
    +. x872 +. x873 +. x874 +. x875 +. x876 +. x877 +. x878 +. x879
    +. x880 +. x881 +. x882 +. x883 +. x884 +. x885 +. x886 +. x887
    +. x888 +. x889 +. x890 +. x891 +. x892 +. x893 +. x894 +. x895
    +. x896 +. x897 +. x898 +. x899 +. x900 +. x901 +. x902 +. x903
    +. x904 +. x905 +. x906 +. x907 +. x908 +. x909 +. x910 +. x911
    +. x912 +. x913 +. x914 +. x915 +. x916 +. x917 +. x918 +. x919
    +. x920 +. x921 +. x922 +. x923 +. x924 +. x925 +. x926 +. x927
    +. x928 +. x929 +. x930 +. x931 +. x932 +. x933 +. x934 +. x935
    +. x936 +. x937 +. x938 +. x939 +. x940 +. x941 +. x942 +. x943
    +. x944 +. x945 +. x946 +. x947 +. x948 +. x949 +. x950 +. x951
    +. x952 +. x953 +. x954 +. x955 +. x956 +. x957 +. x958 +. x959
    +. x960 +. x961 +. x962 +. x963 +. x964 +. x965 +. x966 +. x967
    +. x968 +. x969 +. x970 +. x971 +. x972 +. x973 +. x974 +. x975
    +. x976 +. x977 +. x978 +. x979 +. x980 +. x981 +. x982 +. x983
    +. x984 +. x985 +. x986 +. x987 +. x988 +. x989 +. x990 +. x991
    +. x992 +. x993 +. x994 +. x995 +. x996 +. x997 +. x998 +. x999
  end

let () =
  match Sys.with_async_exns (fun () -> f max_int) with
  | _ -> assert false
  | exception Stack_overflow -> print_endline "Stack_overflow"
