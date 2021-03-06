      SUBROUTINE iau_AE2HD (AZ, EL, PHI, HA, DEC)
*+
*  - - - - - - - - - -
*   i a u _ A E 2 H D
*  - - - - - - - - - -
*
*  Horizon to equatorial coordinates:  transform azimuth and altitude
*  to hour angle and declination.
*
*  This routine is part of the International Astronomical Union's
*  SOFA (Standards of Fundamental Astronomy) software collection.
*
*  Status:  support routine.
*
*  Given:
*     AZ       d     azimuth
*     EL       d     elevation
*     PHI      d     observatory latitude
*
*  Returned:
*     HA       d     hour angle
*     DEC      d     declination
*
*  Notes:
*
*  1)  All the arguments are angles in radians.
*
*  2)  The sign convention for azimuth is north zero, east +pi/2.
*
*  3)  HA is returned in the range +/-pi.  Declination is returned in
*      the range +/-pi/2.
*
*  4)  The latitude PHI is pi/2 minus the angle between the Earth's
*      rotation axis and the adopted zenith.  In many applications it
*      will be sufficient to use the published geodetic latitude of the
*      site.  In very precise (sub-arcsecond) applications, PHI can be
*      corrected for polar motion.
*
*  5)  The azimuth AZ must be with respect to the rotational north pole,
*      as opposed to the ITRS pole, and an azimuth with respect to north
*      on a map of the Earth's surface will need to be adjusted for
*      polar motion if sub-arcsecond accuracy is required.
*
*  6)  Should the user wish to work with respect to the astronomical
*      zenith rather than the geodetic zenith, PHI will need to be
*      adjusted for deflection of the vertical (often tens of
*      arcseconds), and the zero point of HA will also be affected.
*
*  7)  The transformation is the same as Ve = Ry(phi-pi/2)*Rz(pi)*Vh,
*      where Ve and Vh are lefthanded unit vectors in the (ha,dec) and
*      (az,el) systems respectively and Rz and Ry are rotations about
*      first the z-axis and then the y-axis.  (n.b. Rz(pi) simply
*      reverses the signs of the x and y components.)  For efficiency,
*      the algorithm is written out rather than calling other utility
*      functions.  For applications that require even greater
*      efficiency, additional savings are possible if constant terms
*      such as functions of latitude are computed once and for all.
*
*  8)  Again for efficiency, no range checking of arguments is carried
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

      DOUBLE PRECISION AZ, EL, PHI, HA, DEC

      DOUBLE PRECISION SA, CA, SE, CE, SP, CP, X, Y, Z, R


*  Useful trig functions.
      SA = SIN(AZ)
      CA = COS(AZ)
      SE = SIN(EL)
      CE = COS(EL)
      SP = SIN(PHI)
      CP = COS(PHI)

*  Az,Alt unit vector.
      X = - CA*CE*SP + SE*CP
      Y = - SA*CE
      Z = CA*CE*CP + SE*SP

*  To spherical.
      R = SQRT(X*X + Y*Y)
      IF ( R.EQ.0D0 ) THEN
         HA = 0D0
      ELSE
         HA = ATAN2(Y,X)
      END IF
      DEC = ATAN2(Z,R)

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
