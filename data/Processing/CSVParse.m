recentDonation = donationdata.CNDOL1;
secondDonation = donationdata.CNDOL2;
thirdDonation = donationdata.CNDOL3;
slope = zeros(99200, 1);
for i = 1:99200
    if ~isnan(recentDonation(i)) & ~isnan(secondDonation(i)) & isnan(thirdDonation(i))
        slope(i) = sign(recentDonation(i) - secondDonation(i));
    elseif ~isnan(recentDonation(i)) & ~isnan(secondDonation(i)) & ~isnan(thirdDonation(i))
        if recentDonation(i) > secondDonation(i) & secondDonation(i) > thirdDonation(i)
            slope(i) = 1;
        elseif recentDonation(i) < secondDonation(i) & secondDonation(i) < thirdDonation(i)
            slope(i) = -1;
        end
    end
end