      SUBROUTINE iau_HD2AE ( HA, DEC, PHI, AZ, EL )
*+
*  - - - - - - - - - -
*   i a u _ H D 2 A E
*  - - - - - - - - - -
*
*  Equatorial to horizon coordinates:  transform hour angle and
*  declination to azimuth and altitude.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     HA       d     hour angle (local)
*     DEC      d     declination
*     PHI      d     site latitude
*
*  Returned:
*     AZ       d     azimuth
*     EL       d     altitude (informally, elevation)
*
*  Notes:
*
*  1)  All the arguments are angles in radians.
*
*  2)  Azimuth is returned in the range 0-2pi;  north is zero, and east
*      is +pi/2.  Altitude is returned in the range +/- pi/2.
*
*  3)  The latitude PHI is pi/2 minus the angle between the Earth's
*      rotation axis and the adopted zenith.  In many applications it
*      will be sufficient to use the published geodetic latitude of the
*      site.  In very precise (sub-arcsecond) applications, PHI can be
*      corrected for polar motion.
*
*  4)  The returned azimuth AZ is with respect to the rotational north
*      pole, as opposed to the ITRS pole, and for sub-arcsecond accuracy
*      will need to be adjusted for polar motion if it is to be with
*      respect to north on a map of the Earth's surface.
*
*  5)  Should the user wish to work with respect to the astronomical
*      zenith rather than the geodetic zenith, PHI will need to be
*      adjusted for deflection of the vertical (often tens of
*      arcseconds), and the zero point of HA will also be affected.
*
*  6)  The transformation is the same as Vh = Rz(pi)*Ry(pi/2-phi)*Ve,
*      where Vh and Ve are lefthanded unit vectors in the (az,el) and
*      (ha,dec) systems respectively and Ry and Rz are rotations about
*      first the y-axis and then the z-axis.  (n.b. Rz(pi) simply
*      reverses the signs of the x and y components.)  For efficiency,
*      the algorithm is written out rather than calling other utility
*      functions.  For applications that require even greater
*      efficiency, additional savings are possible if constant terms
*      such as functions of latitude are computed once and for all.
*
*  7)  Again for efficiency, no range checking of arguments is carried
*      out.
*
*  Last revision:   2018 January 2
*
*  SOFA release 2021-05-12
*
*  Copyright (C) 2021 IAU SOFA Board.  See notes at end.
*
*-----------------------------------------------------------------------

      IMPLICIT NONE

      DOUBLE PRECISION HA, DEC, PHI, AZ, EL

      DOUBLE PRECISION D2PI
      PARAMETER ( D2PI = 6.283185307179586476925287D0 )

      DOUBLE PRECISION SH, CH, SD, CD, SP, CP, X, Y, Z, R, A


*  Useful trig functions.
      SH = SIN(HA)
      CH = COS(HA)
      SD = SIN(DEC)
      CD = COS(DEC)
      SP = SIN(PHI)
      CP = COS(PHI)

*  Az,Alt unit vector.
      X = - CH*CD*SP + SD*CP
      Y = - SH*CD
      Z = CH*CD*CP + SD*SP

*  To spherical.
      R = SQRT(X*X + Y*Y)
      IF ( R.EQ.0D0 ) THEN
         A = 0D0
      ELSE
         A = ATAN2(Y,X)
      END IF
      IF ( A.LT.0D0 ) A = A+D2PI
      AZ = A
      EL = ATAN2(Z,R)

*  Finished.

*+----------------------------------------------------------------------
*
*  Copyright (C) 2021
*  Standards Of Fundamental Astronomy Board
*  of the International Astronomical Union.
*
*  =====================
*  SOFA Software License
*  =====================
*
*  NOTICE TO USER:
*
*  BY USING THIS SOFTWARE YOU ACCEPT THE FOLLOWING SIX TERMS AND
*  CONDITIONS WHICH APPLY TO ITS USE.
*
*  1. The Software is owned by the IAU SOFA Board ("SOFA").
*
*  2. Permission is granted to anyone to use the SOFA software for any
*     purpose, including commercial applications, free of charge and
*     without payment of royalties, subject to the conditions and
*     restrictions listed below.
*
*  3. You (the user) may copy and distribute SOFA source code to others,
*     and use and adapt its code and algorithms in your own software,
*     on a world-wide, royalty-free basis.  That portion of your
*     distribution that does not consist of intact and unchanged copies
*     of SOFA source code files is a "derived work" that must comply
*     with the following requirements:
*
*     a) Your work shall be marked or carry a statement that it
*        (i) uses routines and computations derived by you from
*        software provided by SOFA under license to you; and
*        (ii) does not itself constitute software provided by and/or
*        endorsed by SOFA.
*
*     b) The source code of your derived work must contain descriptions
*        of how the derived work is based upon, contains and/or differs
*        from the original SOFA software.
*
*     c) The names of all routines in your derived work shall not
*        include the prefix "iau" or "sofa" or trivial modifications
*        thereof such as changes of case.
*
*     d) The origin of the SOFA components of your derived work must
*        not be misrepresented;  you must not claim that you wrote the
*        original software, nor file a patent application for SOFA
*        software or algorithms embedded in the SOFA software.
*
*     e) These requirements must be reproduced intact in any source
*        distribution and shall apply to anyone to whom you have
*        granted a further right to modify the source code of your
*        derived work.
*
*     Note that, as originally distributed, the SOFA software is
*     intended to be a definitive implementation of the IAU standards,
*     and consequently third-party modifications are discouraged.  All
*     variations, no matter how minor, must be explicitly marked as
*     such, as explained above.
*
*  4. You shall not cause the SOFA software to be brought into
*     disrepute, either by misuse, or use for inappropriate tasks, or
*     by inappropriate modification.
*
*  5. The SOFA software is provided "as is" and SOFA makes no warranty
*     as to its use or performance.   SOFA does not and cannot warrant
*     the performance or results which the user may obtain by using the
*     SOFA software.  SOFA makes no warranties, express or implied, as
*     to non-infringement of third party rights, merchantability, or
*     fitness for any particular purpose.  In no event will SOFA be
*     liable to the user for any consequential, incidental, or special
*     damages, including any lost profits or lost savings, even if a
*     SOFA representative has been advised of such damages, or for any
*     claim by any third party.
*
*  6. The provision of any version of the SOFA software under the terms
*     and conditions specified herein does not imply that future
*     versions will also be made available under the same terms and
*     conditions.
*
*  In any published work or commercial product which uses the SOFA
*  software directly, acknowledgement (see www.iausofa.org) is
*  appreciated.
*
*  Correspondence concerning SOFA software should be addressed as
*  follows:
*
*      By email:  sofa@ukho.gov.uk
*      By post:   IAU SOFA Center
*                 HM Nautical Almanac Office
*                 UK Hydrographic Office
*                 Admiralty Way, Taunton
*                 Somerset, TA1 2DN
*                 United Kingdom
*
*-----------------------------------------------------------------------

      END
