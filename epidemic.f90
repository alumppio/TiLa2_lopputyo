program epidemic
	use satunnaismod
	implicit none
     	integer :: i, j, ios1, ios2			! Satunnaisia käytössä olevia kokonaislukuja
       	integer :: ter_t, sai_t, imm_t 		! Sairatietoihin käytettäviä kokonaislukuja
	integer, parameter :: m=200		! Kävelijöiden määrä
	integer, parameter :: n=50		! Laatikon sivun pituus
	integer, parameter :: sairaat=2		! Alussa sairaiden määrä
	integer, parameter :: immuunit=0	! Alussa tautiin immuunien määrä
	integer, parameter :: aika_askel=200	! Kuinka monta kertaa kävelijät liikkuvat
	integer :: siemen(1234)

	type (dalsijat) :: kvj(m) 	! kvj(M) = kävelijäM[x,y,s], missä sijainti(x,y) ja status(s)
					! M on kävelijöiden määrä.

	siemen=1234
	call random_seed(put=siemen) 	! Asetetaan siemen
	

	call luodallaajat(kvj,m,n,sairaat,immuunit) 
	! Aliohjelma luodallaajat(z,m,n,sairas,immuunit), missä z on dalsijat tyypin muuttujan nimi 
	! m on kävelijöiden määrä, n on sivun pituus, sairas on sairaiden määrä alussa ja immuunit 
	! on sairauteen immuunien määrä alussa.

	open(1,file='kavelijat.txt',action='readwrite',iostat=ios1,status='replace')
	if (ios1/=0) then 
		print '(a,a,a)', "Virhe tiedoston kavelijat.txt avaamisessa"
		stop
	endif
	! Kävelijöiden tason kuvaajan tiedot. Tarkistetaan, että tiedoston avaus onnistui.
	
	
	open(2,file='sairastiedot.txt',action='readwrite',iostat=ios2,status='replace')	 
       	if (ios2/=0) then 
		print '(a,a,a)', "Virhe tiedoston sairastiedot.txt avaamisessa"
		stop
	endif
	! Kävelijöiden sairastiedot. Tarkistetaan, että tiedoston avaus onnistui.


	! Kirjoitetaan kävelijöiden tiedot XYZ-formaatissa tason kuvaajaa varten.
	write (1,*) m			! Kävelijöiden määrä
	write (1,*) "#Askel0"		! Aika-askel
	write (1,'(3i5)') kvj(:)	! Kävelijöiden tiedot


	ter_t=0			! Terveiden määrä
	sai_t=0			! Sairaiden määrä
	imm_t=0			! Immuunien määrä

	! Lasketaan kunkin tilan kävelijöiden määrä

	do j=1,m
		if (kvj(j)%s==0) then
			ter_t=ter_t+1
		elseif (kvj(j)%s==1) then
			sai_t=sai_t+1
		elseif (kvj(j)%s==2) then
			imm_t=imm_t+1
		endif
	enddo


	! Kirjoitetaan tiedot kuvaajan tekstitiedostoon
	write (2,'(4i8)') 0,ter_t,sai_t,imm_t
		

	! Liikutetaan kävelijöitä aika-askeleen verran
	do i=1,aika_askel
		call liikkuu(kvj,m,n)	!Kävelijät liikkuvat aliohjelman avulla


		! Kirjoitetaan muuttuneet tiedot jälleen XYZ-formaatissa
		write (1,*) m
		write (1,'(a,i0)') "#Askel",i
		write (1,'(3i5)') kvj(:) 

		ter_t=0
		sai_t=0
		imm_t=0
	
		! Lasketaan uudelleen kunkin tilan käveliöiden määrä
		do j=1,m
			if (kvj(j)%s==0) then
				ter_t=ter_t+1
			elseif (kvj(j)%s==1) then
				sai_t=sai_t+1
			elseif (kvj(j)%s==2) then
				imm_t=imm_t+1
			endif
		enddo
		
		! Kirjataan ylös kävelijöiden muuttuneet sairastiedot
		write (2,'(4i8)') i,ter_t,sai_t,imm_t
	enddo

	! Suljetaan tiedostot ja pidetään ne 
	close(1,status='keep')
	close(2,status='keep')

end program epidemic
