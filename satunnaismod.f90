module satunnaismod
      	implicit none
	real :: r	! satunnainen numero
	real, parameter :: p_tar=0.9	! Tarttumisen todennäköisyys
	real, parameter :: p_par=0.01	! Parantumisen todennäköisyys
	integer, private  :: i,j	! Luuppeja varten kokonaislukumuuttujia
	
	type :: dalsijat		! Kävelijöiden oma muuttuja
		integer :: x,y,s	! Paikka (x,y) ja terveydentila s
	end type dalsijat

contains
	

	 subroutine luodallaajat(z,m,n,sairas,immuuni)	! Alustetaan kävelijät tasolle

		type (dalsijat), intent(inout) :: z(m)	! Itse kävelijät
		integer, intent(in) :: m		! Kävelijöiden määrä
		integer, intent(in) :: n		! Sivun pituus
		integer, intent(in) :: sairas 		! Alussa sairastuneiden määrä
		integer, intent(in) :: immuuni		! Alussa immuunit sairauteen

		! Satunnaisen numeron avulla saadaan kävelijöille koordinaatit


		do i=1,m		
			call random_number(r)
			z(i)%x=1+floor((n+1-1)*r)	! Satunnainen numero väliltä [1,n]
			
			call random_number(r)
			z(i)%y=1+floor((n+1-1)*r)	! Satunnainen numero väliltä [1,n]
			
			if (i<=sairas) then		! Tehdään ensimmäisistä kävelijöistä
				z(i)%s=1		! muuttujan=sairas määrä sairaita

			elseif (i<=(sairas+immuuni)) then
				z(i)%s=2		! Muuttujan=immuunit määrä immuuneja  
							! sairauteen alussa. Lasketaan muuttujan
							! sairas avulla.
			else
				z(i)%s=0		! Loput terveitä
			endif 
		enddo
		
	end subroutine luodallaajat



	subroutine liikkuu(z,k,n)			! Aliohjelma kävelijöiden liikkumiseen
		implicit none
		type(dalsijat), intent(inout) :: z(k)	! Kävelijöiden tieto
		integer, intent(in) :: k		! Kävelijöiden määrä
		integer, intent(in) :: n 		! Sivun pituus


		do i=1,k  
			
		!  Kutsutaan satunnainen numero väliltä [0,1[ ja liikutaan tiettyyn 
		!  suuntaan riippuen satunnaisen numeron arvosta. Jaetaan väli neljään 
		!  tasaiseen väliin, jolloin on yhtä todennäköistä, että kävelijä liikkuu
		!  mihin tahansa suuntaan.

			call random_number(r)

			if (0<=r.and.r<0.25) then 	! Liikutaan -x suuntaan
				z(i)%x=z(i)%x-1
				
				if (z(i)%x==0) then	! Tarkistetaan, että ei mennä yli rajan
					z(i)%x=n
				endif

			elseif (0.25<=r.and.r<0.5) then	! Liikutaan +x suuntaan

				z(i)%x=mod(z(i)%x,n)+1 	! Lasketaan ja samalla tarkistetaan, että
							! ei mennä rajan yli

			elseif (0.5<=r.and.r<0.75) then	! Liikutaan -y suuntaan

				z(i)%y=z(i)%y-1

				if (z(i)%y==0) then	! Tarkistetaan, että ei mennä yli rajan
					z(i)%y=n
				endif

			elseif (0.75<=r.and.r<1) then 	! Liikutaan +y suuntaan

				z(i)%y=mod(z(i)%y,n)+1	!Lasketaan ja samalla tarkistetaan, että
							! ei mennä rajan yli

			endif

		enddo 
		!  Tarkastetaan onko kävelijöitä samoilla laatoilla, mikäli on, kävelijä 
		!  sairastuvat p_t todennäköisyydellä

		do i=1,k
			do j=(i+1),k
				if (z(i)%x==z(j)%x) then
					if (z(i)%y==z(j)%y) then
						if (z(i)%s<1.and.z(j)%s==1) then
							call random_number(r)
							if (r<=p_tar) then   	!jos r<0.01
								z(i)%s=1
							endif
						elseif (z(j)%s<1.and.z(i)%s==1) then 
							call random_number(r)
							if (r<=p_tar) then	!jos r<0.01
								z(j)%s=1
							endif
						endif
					endif
				endif
			enddo
		enddo
		
		!  Tässä käydään läpi jokainen pari kahden do-luupin avulla. Jos x-koordinaattit
		!  ovat samat tarkastetaan onko y-koordinaatit samat. Mikäli näin on, niin
		!  tarkastellaan kävelijöiden terveydentilat. Jos jompikumpi on sairas, niin
		!  Terve sairastuu p_t todennäköisyydellä


		do i=1,k
			if (z(i)%s==1) then
				call random_number(r)
				if (r<=p_par) then	! Parantumisen todennäköisyys p_par
					z(i)%s=2
				endif
			endif
		enddo
		
		!  Jokainen sairastunut kävelijä voi saada immuniteetin p_par todennäköisyydellä.
		!  Käydään jokainen sairastunut kävelijä läpi do-luupin avulla.

	end subroutine liikkuu



end module satunnaismod
