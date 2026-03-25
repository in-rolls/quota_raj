# 01e_up_create_block_crosswalk.R
# Comprehensive District + Block Crosswalk for UP
#
# Maps ALL election district+block combinations (2005, 2010)
# to LGD block codes, enabling GP-level matching.
#
# Following Rajasthan's 01e_raj_create_samiti_crosswalk.R pattern

library(tidyverse)
library(arrow)
library(here)

source(here("scripts/00_utils.R"))

cat("=== Creating Comprehensive UP Block Crosswalk ===\n\n")

# =============================================================================
# STEP 1: Load LGD block reference data
# =============================================================================

lgd_blocks <- read_csv(here("data/crosswalks/lgd_up_blocks.csv"),
                       show_col_types = FALSE) %>%
    select(block_code, block_name, zp_code, zp_name) %>%
    distinct() %>%
    arrange(zp_name, block_name)

cat("LGD blocks loaded:", nrow(lgd_blocks), "\n")

# =============================================================================
# STEP 2: Load district crosswalk
# =============================================================================

district_xwalk <- read_csv(here("data/crosswalks/up_district_xwalk.csv"),
                           show_col_types = FALSE)

cat("District crosswalk loaded:", nrow(district_xwalk), "entries\n")

# =============================================================================
# STEP 3: Define comprehensive manual mapping
# =============================================================================
# Match types:
#   exact        - Direct name match
#   spelling     - Minor spelling variant
#   cross_dist   - Block moved to different district (administrative reorganization)
#   alt_name     - Alternate/historical name
#   urban_ward   - Urban ward subdivision (not block panchayat - excluded)

block_xwalk <- tribble(
    ~elex_district, ~elex_block, ~lgd_district, ~lgd_block_name, ~lgd_block_code, ~match_type,
    # =========================================================================
    # AGRA (District Code 101)
    # =========================================================================
    "Agra", "Achnera", "Agra", "Achhnera", 1386, "spelling",
    "Agra", "Akolaa", "Agra", "Akola", 1387, "spelling",
    "Agra", "Bah", "Agra", "Bah", 1388, "exact",
    "Agra", "Baroli Ahir", "Agra", "Barauli Ahir", 1389, "spelling",
    "Agra", "Bichpuri", "Agra", "Bichpuri", 1390, "exact",
    "Agra", "Etmaadpur", "Agra", "Etmadpur", 1391, "spelling",
    "Agra", "Fatehabad", "Agra", "Fatehabad", 1392, "exact",
    "Agra", "Fatehpur Seekri", "Agra", "Fatehpur Sikri", 1393, "spelling",
    "Agra", "Jagner", "Agra", "Jagner", 1394, "exact",
    "Agra", "Jaitpur Kalan", "Agra", "Jaitpur Kalan", 1395, "exact",
    "Agra", "Khandoli", "Agra", "Khandauli", 1396, "spelling",
    "Agra", "Kheragarh", "Agra", "Kheragarh", 1397, "exact",
    "Agra", "Pinahat", "Agra", "Pinahat", 1398, "exact",
    "Agra", "Shamshabaad", "Agra", "Shamsabad", 1400, "spelling",

    # =========================================================================
    # ALLAHABAD → PRAYAGRAJ (District Code 164)
    # =========================================================================
    "Allahabad", "Bahadurpur", "Prayagraj", "Bahadurpur", 1413, "exact",
    "Allahabad", "Baharia", "Prayagraj", "Bahria", 1414, "spelling",
    "Allahabad", "Chaka", "Prayagraj", "Chaka", 1415, "exact",
    "Allahabad", "Dhanupur", "Prayagraj", "Dhanupur", 1416, "exact",
    "Allahabad", "Handiya", "Prayagraj", "Handia", 1417, "spelling",
    "Allahabad", "Holagarh", "Prayagraj", "Holagarh", 1418, "exact",
    "Allahabad", "Jasra", "Prayagraj", "Jasra", 1419, "exact",
    "Allahabad", "Karachna", "Prayagraj", "Karchhana", 1420, "spelling",
    "Allahabad", "Kaudhiyara", "Prayagraj", "Kaudhiyara", 1421, "exact",
    "Allahabad", "Kaudihar", "Prayagraj", "Kaurihar", 1422, "spelling",
    "Allahabad", "Koraon", "Prayagraj", "Koraon", 1423, "exact",
    "Allahabad", "Manda", "Prayagraj", "Manda", 1424, "exact",
    "Allahabad", "Mau Aima", "Prayagraj", "Mauaima", 1425, "spelling",
    "Allahabad", "Meja", "Prayagraj", "Meja", 1426, "exact",
    "Allahabad", "Phoolpur", "Prayagraj", "Phulpur", 1427, "spelling",
    "Allahabad", "Pratapur", "Prayagraj", "Pratappur", 1428, "spelling",
    "Allahabad", "Saidabad", "Prayagraj", "Saidabad", 1429, "exact",
    "Allahabad", "Shankaragarh", "Prayagraj", "Shankargarh", 1430, "spelling",
    "Allahabad", "Sorav", "Prayagraj", "Soraon", 1431, "spelling",
    "Allahabad", "Uruwa", "Prayagraj", "Uruwan", 1432, "spelling",

    # =========================================================================
    # AMBEDKAR NAGAR (District Code 103)
    # =========================================================================
    "Ambedkar Nagar", "Akbarpur", "Ambedkar Nagar", "Akbarpur", 1433, "exact",
    "Ambedkar Nagar", "Basakhari", "Ambedkar Nagar", "Baskhari", 1434, "spelling",
    "Ambedkar Nagar", "Bhiti", "Ambedkar Nagar", "Bhiti", 1435, "exact",
    "Ambedkar Nagar", "Jahangirganj", "Ambedkar Nagar", "Jahangir Ganj", 1437, "spelling",
    "Ambedkar Nagar", "Jalalpur", "Ambedkar Nagar", "Jalal Pur", 1438, "spelling",
    "Ambedkar Nagar", "Katehri", "Ambedkar Nagar", "Katehari", 1439, "spelling",
    "Ambedkar Nagar", "Miyan", "Ambedkar Nagar", "Bhiyawan", 1436, "alt_name",
    "Ambedkar Nagar", "Ramnagar", "Ambedkar Nagar", "Ram Nagar", 1440, "spelling",
    "Ambedkar Nagar", "Tanda", "Ambedkar Nagar", "Tanda", 1441, "exact",

    # =========================================================================
    # AURAIYA (District Code 106)
    # =========================================================================
    "Auraiya", "Achhaldaa", "Auraiya", "Achchalda", 1442, "spelling",
    "Auraiya", "Ajitmal", "Auraiya", "Ajitmal", 1443, "exact",
    "Auraiya", "Auraiya", "Auraiya", "Auraiya", 1444, "exact",
    "Auraiya", "Bhagyanagar", "Auraiya", "Bhagyanagar", 1445, "exact",
    "Auraiya", "Eva Katra", "Auraiya", "Erwa Katra", 1447, "spelling",
    "Auraiya", "Sahar", "Auraiya", "Sahar", 1448, "exact",
    "Auraiya", "Vidhuna", "Auraiya", "Bidhuna", 1446, "spelling",

    # =========================================================================
    # BAGPAT → BAGHPAT (District Code 107)
    # =========================================================================
    "Bagpat", "Bagpat", "Baghpat", "Baghpat", 1471, "spelling",
    "Bagpat", "Baraut", "Baghpat", "Baraut", 1472, "exact",
    "Bagpat", "Binauli", "Baghpat", "Binauli", 1473, "exact",
    "Bagpat", "Chhaparauli", "Baghpat", "Chhaprauli", 1474, "spelling",
    "Bagpat", "Khekhada", "Baghpat", "Khekra", 1475, "spelling",
    "Bagpat", "Pilana", "Baghpat", "Pilana", 1476, "exact",

    # =========================================================================
    # BAHRAICH (District Code 108)
    # =========================================================================
    "Bahraich", "Balha", "Bahraich", "Balaha", 1477, "spelling",
    "Bahraich", "Fakhrpur", "Bahraich", "Phakharpur", 1485, "spelling",
    "Bahraich", "Huzurpur", "Bahraich", "Huzoorpur", 1479, "spelling",
    "Bahraich", "Jarwal", "Bahraich", "Jarwal", 1480, "exact",
    "Bahraich", "Kaiserganj", "Bahraich", "Kaisarganj", 1481, "spelling",
    "Bahraich", "Mahsi", "Bahraich", "Mahasi", 1482, "spelling",
    "Bahraich", "Mihipurwa", "Bahraich", "Mihinpurwa", 1483, "spelling",
    "Bahraich", "Nawabganj", "Bahraich", "Nawabganj", 1484, "exact",
    "Bahraich", "Prayagpur", "Bahraich", "Prayagpur", 1486, "exact",
    "Bahraich", "Risia", "Bahraich", "Risia", 1487, "exact",
    "Bahraich", "Shivpur", "Bahraich", "Shivpur", 1488, "exact",
    "Bahraich", "Tajwapur", "Bahraich", "Tajwapur", 1489, "exact",
    "Bahraich", "Visheshwarganj", "Bahraich", "Visheshwarganj", 1490, "exact",

    # =========================================================================
    # BALIYA → BALLIA (District Code 109)
    # =========================================================================
    "Baliya", "Baasadeeh", "Ballia", "Bansdih", 1492, "spelling",
    "Baliya", "Bairiya", "Ballia", "Bairia", 1491, "spelling",
    "Baliya", "Belhari", "Ballia", "Belhari", 1493, "exact",
    "Baliya", "Berua Bari", "Ballia", "Beruarbari", 1494, "spelling",
    "Baliya", "Chilakhar", "Ballia", "Chilkahar", 1495, "spelling",
    "Baliya", "Gadwar", "Ballia", "Garwar", 1497, "spelling",
    "Baliya", "Hanumanganj", "Ballia", "Hanumanganj", 1498, "exact",
    "Baliya", "Maniyar", "Ballia", "Maniar", 1499, "spelling",
    "Baliya", "Murli Chappa", "Ballia", "Murlichhapra", 1500, "spelling",
    "Baliya", "Nagra", "Ballia", "Nagra", 1501, "exact",
    "Baliya", "Nawanagar", "Ballia", "Navanagar", 1502, "spelling",
    "Baliya", "Pandah", "Ballia", "Pandah", 1503, "exact",
    "Baliya", "Rasra", "Ballia", "Rasra", 1504, "exact",
    "Baliya", "Revti", "Ballia", "Reoti", 1505, "spelling",
    "Baliya", "Sier", "Ballia", "Siar", 1506, "spelling",
    "Baliya", "Sohaon", "Ballia", "Sohanv", 1507, "spelling",

    # =========================================================================
    # BALRAMPUR (District Code 110)
    # =========================================================================
    "Balrampur", "Balrampur", "Balrampur", "Balrampur", 1508, "exact",
    "Balrampur", "Gaidas Buzurg", "Balrampur", "Gaindas Bujurg", 1509, "spelling",
    "Balrampur", "Gainsadi", "Balrampur", "Gaisri", 1510, "spelling",
    "Balrampur", "Hariya Satgharwa", "Balrampur", "Harriya Satgharwa", 1511, "spelling",
    "Balrampur", "Pachpedwa", "Balrampur", "Pachpedwa", 1512, "exact",
    "Balrampur", "Rehra Bazar", "Balrampur", "Rehera Bazaar", 1513, "spelling",
    "Balrampur", "Shridattganj", "Balrampur", "Shriduttganj", 1514, "spelling",
    "Balrampur", "Tulsipur", "Balrampur", "Tulsipur", 1515, "exact",
    "Balrampur", "Utraula", "Balrampur", "Utraula", 1516, "exact",

    # =========================================================================
    # BANDA (District Code 111)
    # =========================================================================
    "Banda", "Baberu", "Banda", "Baberu", 1517, "exact",
    "Banda", "Badokhar Khurd", "Banda", "Badokhar Khurd", 1518, "exact",
    "Banda", "Bissanda", "Banda", "Bisanda", 1519, "spelling",
    "Banda", "Jaspura", "Banda", "Jaspura", 1520, "exact",
    "Banda", "Kamasin", "Banda", "Kamasin", 1521, "exact",
    "Banda", "Mahua", "Banda", "Mahuva", 1522, "spelling",
    "Banda", "Narani", "Banda", "Naraini", 1523, "spelling",
    "Banda", "Tindwaari", "Banda", "Tindwari", 1524, "spelling",

    # =========================================================================
    # BARABANKI (District Code 112)
    # =========================================================================
    "Barabanki", "Banikoder", "Barabanki", "Bani Kodar", 1525, "spelling",
    "Barabanki", "Banki", "Barabanki", "Banki", 1526, "exact",
    "Barabanki", "Dariyabad", "Barabanki", "Dariyabad", 1527, "exact",
    "Barabanki", "Dewan", "Barabanki", "Dewa", 1528, "spelling",
    "Barabanki", "Haidarganj", "Barabanki", "Haidargarh", 1530, "spelling",
    "Barabanki", "Harakh", "Barabanki", "Harakh", 1531, "exact",
    "Barabanki", "Nindur", "Barabanki", "Nindaura", 1533, "spelling",
    "Barabanki", "Puredlai", "Barabanki", "Puredalai", 1534, "spelling",
    "Barabanki", "Ramnagar", "Barabanki", "Ramnagar", 1535, "exact",
    "Barabanki", "Siddhaur", "Barabanki", "Sidhaur", 1537, "spelling",
    "Barabanki", "Sirauli Gauspur", "Barabanki", "Sirauli Gauspur", 1538, "exact",
    "Barabanki", "Suratganj", "Barabanki", "Suratganj", 1539, "exact",
    "Barabanki", "Trivediganj", "Barabanki", "Trivediganj", 1540, "exact",
    "Barabanki", "masauli", "Barabanki", "Masauli", 1532, "spelling",

    # =========================================================================
    # BAREILLY (District Code 113)
    # =========================================================================
    "Bareilly", "Alampur Jafrabaad", "Bareilly", "Aalampur Jafarabad", 1541, "spelling",
    "Bareilly", "Bahedi", "Bareilly", "Baheri", 1542, "spelling",
    "Bareilly", "Bhadpura", "Bareilly", "Bhadpura", 1543, "exact",
    "Bareilly", "Bhojipura", "Bareilly", "Bhojipura", 1544, "exact",
    "Bareilly", "Bhuta", "Bareilly", "Bhuta", 1545, "exact",
    "Bareilly", "Damkhoda", "Bareilly", "Damkhauda", 1547, "spelling",
    "Bareilly", "Faridpur", "Bareilly", "Faridpur", 1548, "exact",
    "Bareilly", "Fatehganj ( P O )", "Bareilly", "Fatehganj West", 1549, "spelling",
    "Bareilly", "Kyara", "Bareilly", "Kyara", 1550, "exact",
    "Bareilly", "Majhgawan", "Bareilly", "Majhgawan", 1551, "exact",
    "Bareilly", "Mirganj", "Bareilly", "Mirganj", 1552, "exact",
    "Bareilly", "Nawabganj", "Bareilly", "Nawabganj", 1553, "exact",
    "Bareilly", "Ramnagar", "Bareilly", "Ramnagar", 1554, "exact",
    "Bareilly", "Shergarh", "Bareilly", "Shergarh", 1555, "exact",
    "Bareilly", "Vithri", "Bareilly", "Bithiri Chainpur", 1546, "alt_name",

    # =========================================================================
    # BASTI (District Code 114)
    # =========================================================================
    "Basti", "Bahadurpur", "Basti", "Bahadurpur", 1556, "exact",
    "Basti", "Bankati", "Basti", "Bankati", 1557, "exact",
    "Basti", "Basti", "Basti", "Basti", 1558, "exact",
    "Basti", "Bikramjot", "Basti", "Vikram Jot", 1569, "spelling",
    "Basti", "Dubaulia", "Basti", "Dubauliya", 1559, "spelling",
    "Basti", "Gour", "Basti", "Gaur", 1560, "spelling",
    "Basti", "Hareya", "Basti", "Harraiya", 1561, "spelling",
    "Basti", "Kaptaiganj", "Basti", "Kaptanganj", 1562, "spelling",
    "Basti", "Kudrah", "Basti", "Kudaraha", 1563, "spelling",
    "Basti", "Parsrampur", "Basti", "Paras Rampur", 1564, "spelling",
    "Basti", "Ramnagar", "Basti", "Ramnagar", 1565, "exact",
    "Basti", "Raudhauli", "Basti", "Rudauli", 1566, "spelling",
    "Basti", "Saltoya Gopalpur", "Basti", "Saltaua Gopal Pur", 1567, "spelling",
    "Basti", "Saugahat", "Basti", "Sau Ghat", 1568, "spelling",

    # =========================================================================
    # BIJNAUR → BIJNOR (District Code 123)
    # =========================================================================
    "Bijnaur", "Afzalgarah", "Bijnor", "Afzalgarh", 1570, "spelling",
    "Bijnaur", "Alhaipur", "Bijnor", "Jalilpur", 1574, "alt_name",
    "Bijnaur", "Haldaur", "Bijnor", "Haldaur(Khari Jhalu)", 1573, "spelling",
    "Bijnaur", "Jalilpur", "Bijnor", "Jalilpur", 1574, "exact",
    "Bijnaur", "Kiratpur", "Bijnor", "Kiratpur", 1575, "exact",
    "Bijnaur", "Kotwali", "Bijnor", "Kotwali", 1576, "exact",
    "Bijnaur", "Mau Pur Devmal", "Bijnor", "Mohammedpur Deomal", 1577, "spelling",
    "Bijnaur", "Nahatour", "Bijnor", "Nehtaur", 1579, "spelling",
    "Bijnaur", "Najibabad", "Bijnor", "Najibabad", 1578, "exact",
    "Bijnaur", "Noorpur", "Bijnor", "Noorpur", 1580, "exact",
    "Bijnaur", "Syohara", "Bijnor", "Budhanpur Seohara", 1571, "alt_name",

    # =========================================================================
    # BUDAUN (District Code 118)
    # Blocks Gunnaur and Junawai moved to Sambhal (created 2011)
    # =========================================================================
    "Budaun", "Aasfpur", "Budaun", "Asafpur", 1582, "spelling",
    "Budaun", "Ambiyapur", "Budaun", "Ambiapur", 1581, "spelling",
    "Budaun", "Bisauli", "Budaun", "Bisauli", 1583, "exact",
    "Budaun", "Dahagawa", "Budaun", "Dahgavan", 1584, "spelling",
    "Budaun", "Dataganj", "Budaun", "Dataganj", 1585, "exact",
    "Budaun", "Gunnaur", "Sambhal", "Gunnaur", 1586, "cross_dist",
    "Budaun", "Islamnagar", "Budaun", "Islamnagar", 1587, "exact",
    "Budaun", "Jagat", "Budaun", "Jagat", 1588, "exact",
    "Budaun", "Junawai", "Sambhal", "Junawai", 1589, "cross_dist",
    "Budaun", "Myau", "Budaun", "Mion", 1590, "spelling",
    "Budaun", "Salarpur", "Budaun", "Salarpur", 1594, "exact",
    "Budaun", "Samarer", "Budaun", "Samrer", 1595, "spelling",
    "Budaun", "Ujhani", "Budaun", "Ujhani", 1596, "exact",
    "Budaun", "Usawan", "Budaun", "Usawan", 1597, "exact",
    "Budaun", "Wazirganj", "Budaun", "Wazirganj", 1598, "exact",

    # =========================================================================
    # BULANDSHAHR (District Code 125)
    # =========================================================================
    "Bulandshahr", "Agauta", "Bulandshahr", "Agauta", 1599, "exact",
    "Bulandshahr", "Anupshahr", "Bulandshahr", "Anupshahr", 1600, "exact",
    "Bulandshahr", "Arniyaan", "Bulandshahr", "Araniya", 1601, "spelling",
    "Bulandshahr", "B. B. Nagar", "Bulandshahr", "Bhawan Bahadur Nagar", 1602, "spelling",
    "Bulandshahr", "Bulandshahr", "Bulandshahr", "Bulandshahr", 1603, "exact",
    "Bulandshahr", "Danpur", "Bulandshahr", "Danpur", 1604, "exact",
    "Bulandshahr", "Dibai", "Bulandshahr", "Dibai", 1605, "exact",
    "Bulandshahr", "Gulawati", "Bulandshahr", "Gulaothi", 1606, "spelling",
    "Bulandshahr", "Jahangirabad", "Bulandshahr", "Jahangirabad", 1607, "exact",
    "Bulandshahr", "Khujra", "Bulandshahr", "Khurja", 1608, "spelling",
    "Bulandshahr", "Lakhauti", "Bulandshahr", "Lakhaothi", 1609, "spelling",
    "Bulandshahr", "Pahasu", "Bulandshahr", "Pahasu", 1610, "exact",
    "Bulandshahr", "Shikarpur", "Bulandshahr", "Shikarpur", 1611, "exact",
    "Bulandshahr", "Sikandrabad", "Bulandshahr", "Sikandrabad", 1612, "exact",
    "Bulandshahr", "Siyana", "Bulandshahr", "Syana", 1613, "spelling",
    "Bulandshahr", "Unchagaon", "Bulandshahr", "Unchagaon", 1614, "exact",

    # =========================================================================
    # CHANDAULI (District Code 126)
    # =========================================================================
    "Chandauli", "Barhani", "Chandauli", "Berahani", 1615, "spelling",
    "Chandauli", "Chahniya", "Chandauli", "Chahniya", 1616, "exact",
    "Chandauli", "Chakia", "Chandauli", "Chakiya", 1617, "spelling",
    "Chandauli", "Dhanapur", "Chandauli", "Dhanapur", 1619, "exact",
    "Chandauli", "Naugadh", "Chandauli", "Naugarh", 1620, "spelling",
    "Chandauli", "Niyamatabad", "Chandauli", "Niyamatabad", 1621, "exact",
    "Chandauli", "Sakaldeeha", "Chandauli", "Sakaldiha", 1623, "spelling",
    "Chandauli", "Shahababganj", "Chandauli", "Sahabganj", 1622, "spelling",

    # =========================================================================
    # CHITRAKOOT (District Code 127)
    # =========================================================================
    "Chitrakoot", "Karwi (Chitrakoot)", "Chitrakoot", "Karwi", 1624, "spelling",
    "Chitrakoot", "Manikpur", "Chitrakoot", "Manikpur", 1625, "exact",
    "Chitrakoot", "Mau", "Chitrakoot", "Mau", 1626, "exact",
    "Chitrakoot", "Pahadi", "Chitrakoot", "Pahari", 1627, "spelling",
    "Chitrakoot", "Ramnagar", "Chitrakoot", "Ramnagar", 1628, "exact",

    # =========================================================================
    # FAIZABAD → AYODHYA (District Code 105)
    # =========================================================================
    "Faizabad", "Amaniganj", "Ayodhya", "Amaniganj", 1668, "exact",
    "Faizabad", "Bikapur", "Ayodhya", "Bikapur", 1669, "exact",
    "Faizabad", "Haringtonganj", "Ayodhya", "Haringatanganj", 1670, "spelling",
    "Faizabad", "Masaudha", "Ayodhya", "Masodha", 1671, "spelling",
    "Faizabad", "Mayabazar", "Ayodhya", "Maya Bazar", 1673, "spelling",
    "Faizabad", "Milkipur", "Ayodhya", "Milkipur", 1674, "exact",
    "Faizabad", "Puraabaajaar", "Ayodhya", "Pura Bazar", 1675, "spelling",
    "Faizabad", "Sohawal", "Ayodhya", "Sohawal", 1676, "exact",
    "Faizabad", "Taroon", "Ayodhya", "Tarun", 1677, "spelling",

    # =========================================================================
    # FATEHPUR (District Code 130)
    # =========================================================================
    "Fatehpur", "Airayan", "Fatehpur", "Airayan", 1685, "exact",
    "Fatehpur", "Amauli", "Fatehpur", "Amauli", 1686, "exact",
    "Fatehpur", "Asothar", "Fatehpur", "Asothar", 1687, "exact",
    "Fatehpur", "Bahua", "Fatehpur", "Bahua", 1688, "exact",
    "Fatehpur", "Bhitaura", "Fatehpur", "Bhitaura", 1689, "exact",
    "Fatehpur", "Devmai", "Fatehpur", "Devmai", 1690, "exact",
    "Fatehpur", "Dhata", "Fatehpur", "Dhata", 1691, "exact",
    "Fatehpur", "Haswa", "Fatehpur", "Haswa", 1692, "exact",
    "Fatehpur", "Hatgham", "Fatehpur", "Hathgaon", 1693, "spelling",
    "Fatehpur", "Khajooha", "Fatehpur", "Khajuha", 1694, "spelling",
    "Fatehpur", "Malwan", "Fatehpur", "Malwan", 1695, "exact",
    "Fatehpur", "Teliani", "Fatehpur", "Telyani", 1696, "spelling",
    "Fatehpur", "Vijayipur", "Fatehpur", "Vijayipur", 1697, "exact",

    # =========================================================================
    # FIROZABAD (District Code 131)
    # =========================================================================
    "Firozabad", "Aaraon", "Firozabad", "Araon", 1698, "spelling",
    "Firozabad", "Eka", "Firozabad", "Eka", 1699, "exact",
    "Firozabad", "Firozabad", "Firozabad", "Firozabad", 1700, "exact",
    "Firozabad", "Hathwant", "Firozabad", "Hathwant", 1701, "exact",
    "Firozabad", "Jasrana", "Firozabad", "Jasrana", 1702, "exact",
    "Firozabad", "Madanpur", "Firozabad", "Madanpur", 1703, "exact",
    "Firozabad", "Narkhi", "Firozabad", "Narkhi", 1704, "exact",
    "Firozabad", "Shikohabad", "Firozabad", "Shikohabad", 1705, "exact",
    "Firozabad", "Tundla", "Firozabad", "Tundla", 1706, "exact",

    # =========================================================================
    # GAUTAM BUDDHA NAGAR (District Code 132)
    # =========================================================================
    "Gautam Buddha Nagar", "Dadri", "Gautam Buddha Nagar", "Dadri", 1708, "exact",
    "Gautam Buddha Nagar", "Dankaur", "Gautam Buddha Nagar", "Dadri", 1708, "alt_name",
    "Gautam Buddha Nagar", "Jewar", "Gautam Buddha Nagar", "Jewar", 1710, "exact",
    "Gautam Buddha Nagar", "Visarkh", "Gautam Buddha Nagar", "Bisrakh", 1707, "spelling",

    # =========================================================================
    # GHAZIABAD (District Code 133)
    # Blocks Dhaulana, Garhmukteshwar, Simbhavli moved to Hapur (created 2011)
    # =========================================================================
    "Ghaziabad", "Bhojpur", "Ghaziabad", "Bhojpur", 1711, "exact",
    "Ghaziabad", "Dhaulana", "Hapur", "Dhaulana", 1712, "cross_dist",
    "Ghaziabad", "Garhmukteshwar", "Hapur", "Garh Mukteshwar", 1713, "cross_dist",
    "Ghaziabad", "Hapur", "Hapur", "Hapur", 1714, "cross_dist",
    "Ghaziabad", "Loni", "Ghaziabad", "Loni", 1715, "exact",
    "Ghaziabad", "Muradnagar", "Ghaziabad", "Muradnagar", 1716, "exact",
    "Ghaziabad", "Rajapur", "Ghaziabad", "Rajapur", 1717, "exact",
    "Ghaziabad", "Simbhavli", "Hapur", "Simbhawali", 1718, "cross_dist",

    # =========================================================================
    # GHAZIPUR (District Code 134)
    # =========================================================================
    "Ghazipur", "Barachwar", "Ghazipur", "Varachakwar", 1732, "spelling",
    "Ghazipur", "Bhadaura", "Ghazipur", "Bhadaura", 1719, "exact",
    "Ghazipur", "Bhavarankol", "Ghazipur", "Bhanwarkol", 1720, "spelling",
    "Ghazipur", "Jakhniya", "Ghazipur", "Jakhania", 1723, "spelling",
    "Ghazipur", "Jamaniya", "Ghazipur", "Zamania", 1734, "spelling",
    "Ghazipur", "Karanda", "Ghazipur", "Karanda", 1724, "exact",
    "Ghazipur", "Kasimabad", "Ghazipur", "Kasimabad", 1725, "exact",
    "Ghazipur", "Manihari", "Ghazipur", "Manihari", 1726, "exact",
    "Ghazipur", "Mardan", "Ghazipur", "Mardah", 1727, "spelling",
    "Ghazipur", "Muhammadabad", "Ghazipur", "Mohammadabad", 1728, "spelling",
    "Ghazipur", "Revatipur", "Ghazipur", "Revatipur", 1729, "exact",
    "Ghazipur", "Sadaat", "Ghazipur", "Sadat", 1730, "spelling",
    "Ghazipur", "Sadar", "Ghazipur", "Ghazipur", 1722, "alt_name",
    "Ghazipur", "Saidpur", "Ghazipur", "Saidpur", 1731, "exact",
    "Ghazipur", "Virano", "Ghazipur", "Virno", 1733, "spelling",

    # =========================================================================
    # GONDA (District Code 135)
    # =========================================================================
    "Gonda", "Belasar", "Gonda", "Belsar", 1736, "spelling",
    "Gonda", "Bhabha njot", "Gonda", "Babhanjot", 1735, "spelling",
    "Gonda", "Chhapia", "Gonda", "Chhapia", 1737, "exact",
    "Gonda", "Etiyathok", "Gonda", "Itiathok", 1740, "spelling",
    "Gonda", "Haladharamuu", "Gonda", "Haldharmau", 1739, "spelling",
    "Gonda", "Jhanjhari", "Gonda", "Jhanjhari", 1741, "exact",
    "Gonda", "Karnailganj", "Gonda", "Colonelganj", 1738, "alt_name",
    "Gonda", "Katra Bazar", "Gonda", "Katra Bazar", 1742, "exact",
    "Gonda", "Manakapur", "Gonda", "Mankapur", 1743, "spelling",
    "Gonda", "Mujehana", "Gonda", "Mujehana", 1744, "exact",
    "Gonda", "Nawabganj", "Gonda", "Nawabganj", 1745, "exact",
    "Gonda", "Pandri Kripal", "Gonda", "Pandri Kripal", 1746, "exact",
    "Gonda", "Parsapur", "Gonda", "Paraspur", 1747, "spelling",
    "Gonda", "Rupaidih", "Gonda", "Rupaideeh", 1748, "spelling",
    "Gonda", "Tarabganj", "Gonda", "Tarabganj", 1749, "exact",
    "Gonda", "Wazirganj", "Gonda", "Wazirganj", 1750, "exact",

    # =========================================================================
    # GORAKHPUR (District Code 136)
    # =========================================================================
    "Gorakhpur", "Barhalganj", "Gorakhpur", "Barhalganj", 1752, "exact",
    "Gorakhpur", "Belghat", "Gorakhpur", "Belghat", 1753, "exact",
    "Gorakhpur", "Bosagaon", "Gorakhpur", "Bansgaon", 1751, "spelling",
    "Gorakhpur", "Brahmpur", "Gorakhpur", "Brahmpur", 1755, "exact",
    "Gorakhpur", "Campier", "Gorakhpur", "Campierganj", 1756, "spelling",
    "Gorakhpur", "Chargaawon", "Gorakhpur", "Chargawan", 1757, "spelling",
    "Gorakhpur", "Gagaha", "Gorakhpur", "Gagaha", 1758, "exact",
    "Gorakhpur", "Gola", "Gorakhpur", "Gola", 1759, "exact",
    "Gorakhpur", "J. O. Kauriya", "Gorakhpur", "Jangal Kaudia", 1760, "spelling",
    "Gorakhpur", "Kauriram", "Gorakhpur", "Kauri Ram", 1761, "spelling",
    "Gorakhpur", "Khajni", "Gorakhpur", "Khajni", 1762, "exact",
    "Gorakhpur", "Khorabaar", "Gorakhpur", "Khorabar", 1763, "spelling",
    "Gorakhpur", "Pali", "Gorakhpur", "Pali", 1764, "exact",
    "Gorakhpur", "Piparoli", "Gorakhpur", "Piprauli", 1766, "spelling",
    "Gorakhpur", "Pipraich", "Gorakhpur", "Pipraich", 1765, "exact",
    "Gorakhpur", "Sahjanwao", "Gorakhpur", "Sahjanawa", 1767, "spelling",
    "Gorakhpur", "Sardarnagar", "Gorakhpur", "Sardarnagar", 1768, "exact",
    "Gorakhpur", "Uruvaon", "Gorakhpur", "Uruwa", 1769, "spelling",

    # =========================================================================
    # HAMIRPUR (District Code 137)
    # =========================================================================
    "Hamirpur", "Gohand", "Hamirpur", "Gohand", 1770, "exact",
    "Hamirpur", "Kurara", "Hamirpur", "Kurara", 1771, "exact",
    "Hamirpur", "Maudha", "Hamirpur", "Maudaha", 1772, "spelling",
    "Hamirpur", "Muskra", "Hamirpur", "Muskara", 1773, "spelling",
    "Hamirpur", "Raath", "Hamirpur", "Rath", 1774, "spelling",
    "Hamirpur", "Sarila", "Hamirpur", "Sarila", 1775, "exact",
    "Hamirpur", "Sumerpur", "Hamirpur", "Sumerpur", 1776, "exact",

    # =========================================================================
    # HARDOI (District Code 139)
    # =========================================================================
    "Hardoi", "Ahirorhi", "Hardoi", "Ahirori", 1777, "spelling",
    "Hardoi", "Barkhani", "Hardoi", "Bharkhani", 1781, "spelling",
    "Hardoi", "Bawan", "Hardoi", "Bawan", 1778, "exact",
    "Hardoi", "Benhadar", "Hardoi", "Behendar", 1779, "spelling",
    "Hardoi", "Bharawan", "Hardoi", "Bharawan", 1780, "exact",
    "Hardoi", "Bilgraam", "Hardoi", "Bilgram", 1782, "spelling",
    "Hardoi", "Hariyawan", "Hardoi", "Hariyawan", 1783, "exact",
    "Hardoi", "Harpalpur", "Hardoi", "Harpalpur", 1784, "exact",
    "Hardoi", "Kachauna", "Hardoi", "Kachauna", 1785, "exact",
    "Hardoi", "Kothawan", "Hardoi", "Kothawan", 1786, "exact",
    "Hardoi", "Madhauganj", "Hardoi", "Madhoganj", 1787, "spelling",
    "Hardoi", "Mallawan", "Hardoi", "Mallawan", 1788, "exact",
    "Hardoi", "Pihani", "Hardoi", "Pihani", 1789, "exact",
    "Hardoi", "Sandi", "Hardoi", "Sandi", 1790, "exact",
    "Hardoi", "Sandila", "Hardoi", "Sandila", 1791, "exact",
    "Hardoi", "Shahabaad", "Hardoi", "Shahabad", 1792, "spelling",
    "Hardoi", "Surusa", "Hardoi", "Sursa", 1793, "spelling",
    "Hardoi", "Tadiyawan", "Hardoi", "Tandiyawan", 1794, "spelling",
    "Hardoi", "Todarpur", "Hardoi", "Todarpur", 1795, "exact",

    # =========================================================================
    # HATHRAS (District Code 140)
    # =========================================================================
    "Hathras", "Hasayan", "Hathras", "Hasayan", 1919, "exact",
    "Hathras", "Mursan", "Hathras", "Mursan", 1921, "exact",
    "Hathras", "Sadabad", "Hathras", "Sadabad", 1922, "exact",
    "Hathras", "Sahpau", "Hathras", "Sehpau", 1924, "spelling",
    "Hathras", "Sasni", "Hathras", "Sasni", 1923, "exact",
    "Hathras", "Sikandra Rao", "Hathras", "Sikandrarao", 1925, "spelling",

    # =========================================================================
    # ITAWAH → ETAWAH (District Code 129)
    # =========================================================================
    "Itawah", "Badh. Pura", "Etawah", "Barhpura", 1660, "spelling",
    "Itawah", "Basrehar", "Etawah", "Basrehar", 1661, "exact",
    "Itawah", "Bharathana", "Etawah", "Bharthana", 1662, "spelling",
    "Itawah", "Chakarnagar", "Etawah", "Chakarnagar", 1663, "exact",
    "Itawah", "Jaswantnagar", "Etawah", "Jaswantnagar", 1664, "exact",
    "Itawah", "Mahewa", "Etawah", "Mahewa", 1665, "exact",
    "Itawah", "Saifai", "Etawah", "Sefai", 1666, "spelling",
    "Itawah", "Takha", "Etawah", "Takha", 1667, "exact",

    # =========================================================================
    # JALAUN (District Code 141)
    # =========================================================================
    "Jalaun", "Dakor", "Jalaun", "Dakore", 1796, "spelling",
    "Jalaun", "Jalaun", "Jalaun", "Jalaun", 1797, "exact",
    "Jalaun", "Kadoura", "Jalaun", "Kadaura", 1798, "spelling",
    "Jalaun", "Kochan", "Jalaun", "Konch", 1799, "spelling",
    "Jalaun", "Kuthound", "Jalaun", "Kuthaund", 1800, "spelling",
    "Jalaun", "Madhogarh", "Jalaun", "Madhogarh", 1801, "exact",
    "Jalaun", "Mahewa", "Jalaun", "Maheva", 1802, "spelling",
    "Jalaun", "Nadigaon", "Jalaun", "Nadigaon", 1803, "exact",
    "Jalaun", "Rampura", "Jalaun", "Rampura", 1804, "exact",

    # =========================================================================
    # JAUNPUR (District Code 142)
    # =========================================================================
    "Jaunpur", "Badlapur", "Jaunpur", "Badla Pur", 1805, "spelling",
    "Jaunpur", "Barsathi", "Jaunpur", "Barasathi", 1807, "spelling",
    "Jaunpur", "Buxa", "Jaunpur", "Baksha", 1806, "spelling",
    "Jaunpur", "Dobhi", "Jaunpur", "Dobhi", 1809, "exact",
    "Jaunpur", "Jalalpur", "Jaunpur", "Jalal Pur", 1810, "spelling",
    "Jaunpur", "Karanja Kala", "Jaunpur", "Karanja Kala", 1811, "exact",
    "Jaunpur", "Kerakat", "Jaunpur", "Kerakat", 1812, "exact",
    "Jaunpur", "Khutahan", "Jaunpur", "Khuthan", 1813, "spelling",
    "Jaunpur", "Machhalishahr", "Jaunpur", "Machchali Shahar", 1814, "spelling",
    "Jaunpur", "Madiyahu", "Jaunpur", "Mariyahu", 1816, "spelling",
    "Jaunpur", "Maharajganj", "Jaunpur", "Maharaj Ganj", 1815, "spelling",
    "Jaunpur", "Muftiganj", "Jaunpur", "Mufti Ganj", 1817, "spelling",
    "Jaunpur", "Mugrabadshahapur", "Jaunpur", "Mungra Badshah Pur", 1818, "spelling",
    "Jaunpur", "Ramnagar", "Jaunpur", "Ram Nagar", 1819, "spelling",
    "Jaunpur", "Rampur", "Jaunpur", "Ram Pur", 1820, "spelling",
    "Jaunpur", "Shahganj Sodhi", "Jaunpur", "Shah Ganj", 1821, "spelling",
    "Jaunpur", "Sikarara", "Jaunpur", "Sikrara", 1822, "spelling",
    "Jaunpur", "Srikoni", "Jaunpur", "Sirkoni", 1823, "spelling",
    "Jaunpur", "Suitha Kala", "Jaunpur", "Suitha Kala", 1824, "exact",
    "Jaunpur", "Sujangang", "Jaunpur", "Sujan Ganj", 1825, "spelling",

    # =========================================================================
    # JHANSHI → JHANSI (District Code 143)
    # =========================================================================
    "Jhanshi", "Babina", "Jhansi", "Babina", 1826, "exact",
    "Jhanshi", "Badagaon", "Jhansi", "Badagaon", 1827, "exact",
    "Jhanshi", "Bamour", "Jhansi", "Bamaur", 1828, "spelling",
    "Jhanshi", "Chirgaon", "Jhansi", "Chirgaon", 1830, "exact",
    "Jhanshi", "Moth", "Jhansi", "Moth", 1833, "exact",

    # =========================================================================
    # JYOTIBA PHULE NAGAR → AMROHA (District Code 104)
    # =========================================================================
    "Jyotiba Phule Nagar", "Amroha", "Amroha", "Amroha", 1834, "exact",
    "Jyotiba Phule Nagar", "Dhanaura", "Amroha", "Dhanaura", 1835, "exact",
    "Jyotiba Phule Nagar", "Gajraula", "Amroha", "Gajraula", 1836, "exact",
    "Jyotiba Phule Nagar", "Gangeshwari", "Amroha", "Gangeshwari", 1837, "exact",
    "Jyotiba Phule Nagar", "Hasanpur", "Amroha", "Hasanpur", 1838, "exact",
    "Jyotiba Phule Nagar", "Joya", "Amroha", "Joya", 1839, "exact",

    # =========================================================================
    # KANNAUJ (District Code 144)
    # =========================================================================
    "Kannauj", "Chhibramau", "Kannauj", "Chhibramau", 1840, "exact",
    "Kannauj", "Gugapur", "Kannauj", "Gughrapur", 1841, "spelling",
    "Kannauj", "Haseran", "Kannauj", "Haseran", 1842, "exact",
    "Kannauj", "Jalalabad", "Kannauj", "Jalalabad", 1843, "exact",
    "Kannauj", "Kannauj", "Kannauj", "Kannauj", 1844, "exact",
    "Kannauj", "Saurkh", "Kannauj", "Saurikh", 1845, "spelling",
    "Kannauj", "Talam", "Kannauj", "Talgram", 1846, "spelling",
    "Kannauj", "Umarda", "Kannauj", "Umarda", 1847, "exact",

    # =========================================================================
    # KANPUR NAGAR (District Code 146)
    # =========================================================================
    "Kanpur Nagar", "Bhitar Gaon", "Kanpur Nagar", "Bhitargaon", 1858, "spelling",
    "Kanpur Nagar", "Bilhaur", "Kanpur Nagar", "Bilhaur", 1859, "exact",
    "Kanpur Nagar", "Chaubepur", "Kanpur Nagar", "Chaubeypur", 1860, "spelling",
    "Kanpur Nagar", "Ghatampur", "Kanpur Nagar", "Ghatampur", 1861, "exact",
    "Kanpur Nagar", "Kakwan", "Kanpur Nagar", "Kakwan", 1862, "exact",
    "Kanpur Nagar", "Kalyanpur", "Kanpur Nagar", "Kalyanpur", 1863, "exact",
    "Kanpur Nagar", "Patara", "Kanpur Nagar", "Patara", 1864, "exact",
    "Kanpur Nagar", "Sarasaul", "Kanpur Nagar", "Sarsol", 1865, "spelling",
    "Kanpur Nagar", "Shivrajpur", "Kanpur Nagar", "Shivrajpur", 1866, "exact",
    "Kanpur Nagar", "Vidhnu", "Kanpur Nagar", "Vidhunu", 1867, "spelling",

    # =========================================================================
    # KAUSHAMBI (District Code 148)
    # =========================================================================
    "Kaushambi", "Chayal", "Kaushambi", "Chail", 1868, "spelling",
    "Kaushambi", "Kara", "Kaushambi", "Kara", 1869, "exact",
    "Kaushambi", "Kaushambi", "Kaushambi", "Kaushambi", 1870, "exact",
    "Kaushambi", "Manjhanpur", "Kaushambi", "Manjhanpur", 1871, "exact",
    "Kaushambi", "Muratganj", "Kaushambi", "Mooratganj", 1872, "spelling",
    "Kaushambi", "NEWADA", "Kaushambi", "Nevada", 1873, "spelling",
    "Kaushambi", "Sarsawa", "Kaushambi", "Sarsawan", 1874, "spelling",
    "Kaushambi", "Sirathu", "Kaushambi", "Sirathu", 1875, "exact",

    # =========================================================================
    # KUSHINAGAR → KUSHI NAGAR (District Code 150)
    # =========================================================================
    "Kushinagar", "Fazilnagar", "Kushi Nagar", "Fazilnagar", 1892, "exact",
    "Kushinagar", "Hata", "Kushi Nagar", "Hata", 1893, "exact",
    "Kushinagar", "Kaptaiganj", "Kushi Nagar", "Kaptainganj", 1894, "spelling",
    "Kushinagar", "Kasaya", "Kushi Nagar", "Kasaya", 1895, "exact",
    "Kushinagar", "Khadda", "Kushi Nagar", "Khadda", 1896, "exact",
    "Kushinagar", "Moti Chak", "Kushi Nagar", "Motichak", 1897, "spelling",
    "Kushinagar", "Nebua Naurangia", "Kushi Nagar", "Nebua Naurangia", 1898, "exact",
    "Kushinagar", "Padrauna", "Kushi Nagar", "Padrauna", 1899, "exact",
    "Kushinagar", "Ramkola", "Kushi Nagar", "Ramkola", 1900, "exact",
    "Kushinagar", "Sewarhi", "Kushi Nagar", "Seorahi", 1901, "spelling",
    "Kushinagar", "Sukaroli", "Kushi Nagar", "Sukrauli", 1902, "spelling",
    "Kushinagar", "Tamkuhiraj", "Kushi Nagar", "Tamkuhiraj", 1903, "exact",
    "Kushinagar", "Vishunpura", "Kushi Nagar", "Vishunpura", 1904, "exact",

    # =========================================================================
    # LAKHIMPUR KHIRI → KHERI (District Code 149)
    # =========================================================================
    "Lakhimpur Khiri", "Bankeganj", "Kheri", "Bankeyganj", 1876, "spelling",
    "Lakhimpur Khiri", "Behjam Devi", "Kheri", "Behjam", 1877, "spelling",
    "Lakhimpur Khiri", "Bijua", "Kheri", "Bijuwa", 1878, "spelling",
    "Lakhimpur Khiri", "Dhaurhara", "Kheri", "Dhaurhara", 1879, "exact",
    "Lakhimpur Khiri", "Gola (Kumbhi)", "Kheri", "Kumbhigola", 1881, "spelling",
    "Lakhimpur Khiri", "Isanagar", "Kheri", "Isanagar", 1880, "exact",
    "Lakhimpur Khiri", "Lakhimpur", "Kheri", "Lakhimpur", 1882, "exact",
    "Lakhimpur Khiri", "Mitauli", "Kheri", "Mitauli", 1883, "exact",
    "Lakhimpur Khiri", "Mohammadi", "Kheri", "Mohammadi", 1884, "exact",
    "Lakhimpur Khiri", "Nakha", "Kheri", "Nakaha", 1885, "spelling",
    "Lakhimpur Khiri", "Nighasan", "Kheri", "Nighasan", 1886, "exact",
    "Lakhimpur Khiri", "Palia", "Kheri", "Palia", 1887, "exact",
    "Lakhimpur Khiri", "Pasgawan", "Kheri", "Pasgawan", 1888, "exact",
    "Lakhimpur Khiri", "Phoolbehad", "Kheri", "Phoolbehar", 1889, "spelling",
    "Lakhimpur Khiri", "Ramiyabehar", "Kheri", "Ramia Behar", 1890, "spelling",

    # =========================================================================
    # LALITPUR (District Code 151)
    # =========================================================================
    "Lalitpur", "Baar", "Lalitpur", "Bar", 1905, "spelling",
    "Lalitpur", "Jakhaura", "Lalitpur", "Jakhaura", 1907, "exact",
    "Lalitpur", "Mahrauni", "Lalitpur", "Mehroni", 1909, "spelling",
    "Lalitpur", "Maraawara", "Lalitpur", "Mandawara", 1908, "spelling",
    "Lalitpur", "Talbehat", "Lalitpur", "Talbehat", 1910, "exact",
    "Lalitpur", "Virddha", "Lalitpur", "Birdha", 1906, "spelling",

    # =========================================================================
    # LUCKNOW (District Code 152)
    # =========================================================================
    "Lucknow", "Chinhat", "Lucknow", "Chinhat", 1912, "exact",
    "Lucknow", "Gosaiganj", "Lucknow", "Gosaiganj", 1913, "exact",
    "Lucknow", "Kakori", "Lucknow", "Kakori", 1914, "exact",
    "Lucknow", "Malihabaad", "Lucknow", "Malihabad", 1916, "spelling",
    "Lucknow", "Mohanlalganj", "Lucknow", "Mohanlalganj", 1917, "exact",
    "Lucknow", "Sarojninagar", "Lucknow", "Sarojaninagar", 1918, "spelling",

    # =========================================================================
    # MAHARAJGANJ (District Code 153)
    # =========================================================================
    "Maharajganj", "Brajamangnaj", "Maharajganj", "Bridgemanganj", 1926, "spelling",
    "Maharajganj", "Dhani", "Maharajganj", "Dhani", 1927, "exact",
    "Maharajganj", "Ferenda", "Maharajganj", "Pharenda", 1936, "spelling",
    "Maharajganj", "Ghughli", "Maharajganj", "Ghughli", 1928, "exact",
    "Maharajganj", "Lakshmipur", "Maharajganj", "Lakshmipur", 1929, "exact",
    "Maharajganj", "Maharajganj Sadar", "Maharajganj", "Mahrajganj", 1930, "spelling",
    "Maharajganj", "Mithoura", "Maharajganj", "Mithaura", 1931, "spelling",
    "Maharajganj", "Nautanwa", "Maharajganj", "Nautanwa", 1932, "exact",
    "Maharajganj", "Nichlaul", "Maharajganj", "Nichlaul", 1933, "exact",
    "Maharajganj", "Paniyara", "Maharajganj", "Paniyara", 1934, "exact",
    "Maharajganj", "Partawal", "Maharajganj", "Partawal", 1935, "exact",
    "Maharajganj", "Siswa Bazar", "Maharajganj", "Siswa", 1937, "spelling",

    # =========================================================================
    # MAHOBA (District Code 154)
    # =========================================================================
    "Mahoba", "Charkhari", "Mahoba", "Charkhari", 1938, "exact",
    "Mahoba", "Jaitpur", "Mahoba", "Jaitpur", 1939, "exact",
    "Mahoba", "Kabrai", "Mahoba", "Kabrai", 1940, "exact",
    "Mahoba", "Panwadi", "Mahoba", "Panwari", 1941, "spelling",

    # =========================================================================
    # MAINPURI (District Code 155)
    # =========================================================================
    "Mainpuri", "Ahir", "Mainpuri", "Karhal", 1946, "alt_name",
    "Mainpuri", "Barnahal", "Mainpuri", "Barnahal", 1942, "exact",
    "Mainpuri", "Bebar", "Mainpuri", "Bewar", 1943, "spelling",
    "Mainpuri", "Ghiror", "Mainpuri", "Ghiror", 1944, "exact",
    "Mainpuri", "Jagir", "Mainpuri", "Jageer", 1945, "spelling",
    "Mainpuri", "Karhal", "Mainpuri", "Karhal", 1946, "exact",
    "Mainpuri", "Kurawli", "Mainpuri", "Kuraoli", 1948, "spelling",
    "Mainpuri", "Mainpuri", "Mainpuri", "Mainpuri", 1949, "exact",
    "Mainpuri", "Sultanganj", "Mainpuri", "Sultanganj", 1950, "exact",

    # =========================================================================
    # MAU (District Code 157)
    # =========================================================================
    "Mau", "Baragaon", "Mau", "Badraon", 1961, "spelling",
    "Mau", "Dohrighat", "Mau", "Dohri Ghat", 1962, "spelling",
    "Mau", "Ghosi", "Mau", "Ghosi", 1964, "exact",
    "Mau", "Kopaganj", "Mau", "Kopaganj", 1965, "exact",
    "Mau", "Muhammadabad", "Mau", "Mohammadabad Gohana", 1966, "spelling",
    "Mau", "Pardha", "Mau", "Pardaha", 1967, "spelling",
    "Mau", "Ranipur", "Mau", "Ranipur", 1968, "exact",
    "Mau", "Ratanpura", "Mau", "Ratanpura", 1969, "exact",

    # =========================================================================
    # MEERUT (District Code 160)
    # =========================================================================
    "Meerut", "Daurala", "Meerut", "Daurala", 1970, "exact",
    "Meerut", "Hastinaapur", "Meerut", "Hastinapur", 1971, "spelling",
    "Meerut", "Jani Khurd", "Meerut", "Janikhurd", 1972, "spelling",
    "Meerut", "Kharkhauda", "Meerut", "Kharkhoda", 1973, "spelling",
    "Meerut", "Machhra", "Meerut", "Machra", 1974, "spelling",
    "Meerut", "Mawana", "Meerut", "Mawana Kalan", 1975, "spelling",
    "Meerut", "Meerut", "Meerut", "Meerut", 1976, "exact",
    "Meerut", "Parikshitgarh", "Meerut", "Parikshitgarh", 1977, "exact",
    "Meerut", "Rajpura", "Meerut", "Rajpura", 1978, "exact",
    "Meerut", "Rohta", "Meerut", "Rohta", 1979, "exact",
    "Meerut", "Sardhana", "Meerut", "Sardhana", 1980, "exact",
    "Meerut", "Sarurpur Khurd", "Meerut", "Sarurpur Khurd", 1981, "exact",

    # =========================================================================
    # MIRZAPUR (District Code 161)
    # =========================================================================
    "Mirzapur", "Chhaanbe", "Mirzapur", "Chhanvey", 1982, "spelling",
    "Mirzapur", "Halia", "Mirzapur", "Hallia", 1983, "spelling",
    "Mirzapur", "Jamalpur", "Mirzapur", "Jamalpur", 1984, "exact",
    "Mirzapur", "Kon (Cheelha)", "Mirzapur", "Kon", 1985, "spelling",
    "Mirzapur", "Lalganj", "Mirzapur", "Lalganj", 1986, "exact",
    "Mirzapur", "Majhwan", "Mirzapur", "Majhawa", 1987, "spelling",
    "Mirzapur", "Narayanpur", "Mirzapur", "Narainpur", 1989, "spelling",
    "Mirzapur", "Pahadi", "Mirzapur", "Pahari", 1990, "spelling",
    "Mirzapur", "Patehara Kala", "Mirzapur", "Patehra", 1991, "spelling",
    "Mirzapur", "Rajgarh", "Mirzapur", "Rajgarh", 1992, "exact",
    "Mirzapur", "Seekhar", "Mirzapur", "Shikhar", 1993, "spelling",
    "Mirzapur", "Seeti", "Mirzapur", "Shikhar", 1993, "alt_name",

    # =========================================================================
    # MURADABAD → MORADABAD (District Code 162)
    # Blocks Sambhal, Bahjoi, Panwasa, Asamoli moved to Sambhal (created 2011)
    # =========================================================================
    "Muradabad", "Asamoli", "Sambhal", "Asmauli", 1994, "cross_dist",
    "Muradabad", "Bahjoi", "Sambhal", "Bahjoi", 1995, "cross_dist",
    "Muradabad", "Bhagatpur", "Moradabad", "Bhagatpur Tanda", 1997, "spelling",
    "Muradabad", "Bilari", "Moradabad", "Bilari", 1998, "exact",
    "Muradabad", "Chhajlait", "Moradabad", "Chhajlet", 1999, "spelling",
    "Muradabad", "Dilari", "Moradabad", "Dilari", 2000, "exact",
    "Muradabad", "Dingarpur (Kundarki)", "Moradabad", "Kundarki", 2001, "alt_name",
    "Muradabad", "Mudhaa Pandey", "Moradabad", "Munda Pandey", 2003, "spelling",
    "Muradabad", "Muradabad", "Moradabad", "Moradabad", 2002, "spelling",
    "Muradabad", "Panwasa", "Sambhal", "Panwasa", 2004, "cross_dist",
    "Muradabad", "Sambhal", "Sambhal", "Sambhal", 2005, "cross_dist",
    "Muradabad", "Thakurdwara", "Moradabad", "Thakurdwara", 2006, "exact",

    # =========================================================================
    # MUZZAFARNAGAR → MUZAFFARNAGAR (District Code 166)
    # Blocks Kairana, Shamli, Un, Kandla moved to Shamli (created 2011)
    # =========================================================================
    "Muzzafarnagar", "Baghra", "Muzaffarnagar", "Baghara", 2007, "spelling",
    "Muzzafarnagar", "Budhana", "Muzaffarnagar", "Budhana", 2008, "exact",
    "Muzzafarnagar", "Charthaawal", "Muzaffarnagar", "Charthawal", 2009, "spelling",
    "Muzzafarnagar", "Jansath", "Muzaffarnagar", "Jansath", 2010, "exact",
    "Muzzafarnagar", "Kairana", "Shamli", "Kairana", 2011, "cross_dist",
    "Muzzafarnagar", "Kandla", "Shamli", "Kandhla", 2012, "cross_dist",
    "Muzzafarnagar", "Khatauli", "Muzaffarnagar", "Khatauli", 2013, "exact",
    "Muzzafarnagar", "Morna", "Muzaffarnagar", "Morna", 2014, "exact",
    "Muzzafarnagar", "Purkaji", "Muzaffarnagar", "Purkaji", 2016, "exact",
    "Muzzafarnagar", "Sadar", "Muzaffarnagar", "Muzaffarnagar", 2015, "alt_name",
    "Muzzafarnagar", "Shahpur", "Muzaffarnagar", "Shahpur", 2017, "exact",
    "Muzzafarnagar", "Shamli", "Shamli", "Shamli", 2018, "cross_dist",
    "Muzzafarnagar", "Thana", "Shamli", "Thana Bhawan", 2019, "spelling",
    "Muzzafarnagar", "Un", "Shamli", "Un", 2020, "cross_dist",

    # =========================================================================
    # PILIBHIT (District Code 162)
    # =========================================================================
    "Pilibhit", "Amariya", "Pilibhit", "Amariya", 2021, "exact",
    "Pilibhit", "Barkheda", "Pilibhit", "Barkhera", 2022, "spelling",
    "Pilibhit", "Bilsanda", "Pilibhit", "Bilsanda", 2023, "exact",
    "Pilibhit", "Bisalpur", "Pilibhit", "Bisalpur", 2024, "exact",
    "Pilibhit", "Lalauri Kheda", "Pilibhit", "Lalaurikhera", 2025, "spelling",
    "Pilibhit", "Marauri", "Pilibhit", "Marori", 2026, "spelling",
    "Pilibhit", "Purnanpur", "Pilibhit", "Puranpur", 2027, "spelling",

    # =========================================================================
    # PRATAPGARH (District Code 163)
    # =========================================================================
    "Pratapgarh", "Asapur", "Pratapgarh", "Aspur Deosara", 2028, "spelling",
    "Pratapgarh", "Baabaa Belkharnath Dhaam", "Pratapgarh", "Baba Belkharnath Dham", 2029, "spelling",
    "Pratapgarh", "Baabaganj", "Pratapgarh", "Babaganj", 2030, "spelling",
    "Pratapgarh", "Bihar", "Pratapgarh", "Bihar", 2031, "exact",
    "Pratapgarh", "Gaura", "Pratapgarh", "Gaura", 2032, "exact",
    "Pratapgarh", "Kalakankar", "Pratapgarh", "Kalakankar", 2033, "exact",
    "Pratapgarh", "Kunda", "Pratapgarh", "Kunda", 2034, "exact",
    "Pratapgarh", "Lakshmanpur", "Pratapgarh", "Lakshamanpur", 2035, "spelling",
    "Pratapgarh", "Lalganj", "Pratapgarh", "Lalganj", 2036, "exact",
    "Pratapgarh", "Magraura", "Pratapgarh", "Magraura", 2037, "exact",
    "Pratapgarh", "Mandhaata", "Pratapgarh", "Mandhata", 2038, "spelling",
    "Pratapgarh", "Patti", "Pratapgarh", "Patti", 2039, "exact",
    "Pratapgarh", "Sadar", "Pratapgarh", "Pratapgarh (Sadar)", 2040, "alt_name",
    "Pratapgarh", "Sandwaa Chandrika", "Pratapgarh", "Sandwa Chandrika", 2042, "spelling",
    "Pratapgarh", "Sangipur", "Pratapgarh", "Sangipur", 2043, "exact",
    "Pratapgarh", "Shivgarh", "Pratapgarh", "Shivgarh", 2044, "exact",

    # =========================================================================
    # RAE BARELI (District Code 165)
    # =========================================================================
    "Rae bareli", "Amawan", "Rae Bareli", "Amawan", 2045, "exact",
    "Rae bareli", "Bachhrawan", "Rae Bareli", "Bachharawan", 2046, "spelling",
    "Rae bareli", "Dalmau", "Rae Bareli", "Dalmau", 2049, "exact",
    "Rae bareli", "Deenshaahgaura", "Rae Bareli", "Deenshah Gaura", 2050, "spelling",
    "Rae bareli", "Harchandpur", "Rae Bareli", "Harchandpur", 2052, "exact",
    "Rae bareli", "Khiro", "Rae Bareli", "Khiron", 2054, "spelling",
    "Rae bareli", "Lalganj", "Rae Bareli", "Lalganj", 2055, "exact",
    "Rae bareli", "Maharajganj", "Rae Bareli", "Mahrajganj", 2056, "spelling",
    "Rae bareli", "Rahi", "Rae Bareli", "Rahi", 2057, "exact",
    "Rae bareli", "Rohanea", "Rae Bareli", "Rohania", 2058, "spelling",
    "Rae bareli", "Sareni", "Rae Bareli", "Sareni", 2060, "exact",
    "Rae bareli", "Sataon", "Rae Bareli", "Sataon", 2061, "exact",
    "Rae bareli", "Shivgarh", "Rae Bareli", "Shivgarh", 2062, "exact",
    "Rae bareli", "Unchahaar", "Rae Bareli", "Unchahar", 2065, "spelling",

    # =========================================================================
    # RAMPUR (District Code 167)
    # =========================================================================
    "Rampur", "Bilaaspur", "Rampur", "Bilaspur", 2066, "spelling",
    "Rampur", "Chamaraua", "Rampur", "Chamraon", 2067, "spelling",
    "Rampur", "Milk", "Rampur", "Milak", 2068, "spelling",
    "Rampur", "Saidnagar", "Rampur", "Saidnagar", 2069, "exact",

    # =========================================================================
    # SAHARANPUR (District Code 168)
    # =========================================================================
    "Saharanpur", "Baliyakhedi", "Saharanpur", "Ballia Kheri", 2072, "spelling",
    "Saharanpur", "Devbanda", "Saharanpur", "Deoband", 2073, "spelling",
    "Saharanpur", "Gangoh", "Saharanpur", "Gangoh", 2074, "exact",
    "Saharanpur", "Muzaffarabad", "Saharanpur", "Muzaffarabad", 2075, "exact",
    "Saharanpur", "Nagal", "Saharanpur", "Nagal", 2076, "exact",
    "Saharanpur", "Nakud", "Saharanpur", "Nakur", 2077, "spelling",
    "Saharanpur", "Nanauta", "Saharanpur", "Nanauta", 2078, "exact",
    "Saharanpur", "Puranka", "Saharanpur", "Puwarka", 2079, "spelling",
    "Saharanpur", "Rampur Maniharan", "Saharanpur", "Rampur Maniharan", 2080, "exact",
    "Saharanpur", "Sadholi Kadim", "Saharanpur", "Sadauli Qadeem", 2081, "spelling",
    "Saharanpur", "Sarsawa", "Saharanpur", "Sarsawan", 2082, "spelling",

    # =========================================================================
    # SANT KABEER NAGAR (District Code 169)
    # =========================================================================
    "Sant Kabeer Nagar", "Baghauli", "Sant Kabeer Nagar", "Baghauli", 2083, "exact",
    "Sant Kabeer Nagar", "Belhar Kala", "Sant Kabeer Nagar", "Belhar Kala", 2084, "exact",
    "Sant Kabeer Nagar", "Hesarbazar", "Sant Kabeer Nagar", "Hainsar Bazar", 2085, "spelling",
    "Sant Kabeer Nagar", "Khalilabad", "Sant Kabeer Nagar", "Khalilabad", 2086, "exact",
    "Sant Kabeer Nagar", "Mehdawal", "Sant Kabeer Nagar", "Mehdawal", 2087, "exact",
    "Sant Kabeer Nagar", "Nathnagar", "Sant Kabeer Nagar", "Nath Nagar", 2088, "spelling",
    "Sant Kabeer Nagar", "Pauli", "Sant Kabeer Nagar", "Pauli", 2089, "exact",
    "Sant Kabeer Nagar", "Sautha", "Sant Kabeer Nagar", "Santha", 2090, "spelling",
    "Sant Kabeer Nagar", "Semriyawa", "Sant Kabeer Nagar", "Semariyawan", 2091, "spelling",

    # =========================================================================
    # SANT RAVIDAS NAGAR (District Code 170)
    # =========================================================================
    "Sant Ravidas Nagar", "Abholi", "Sant Ravidas Nagar", "Abhauli", 2092, "spelling",
    "Sant Ravidas Nagar", "Aurain", "Sant Ravidas Nagar", "Aurai", 2093, "spelling",
    "Sant Ravidas Nagar", "Bhadohi", "Sant Ravidas Nagar", "Bhadohi", 2094, "exact",
    "Sant Ravidas Nagar", "Deegh", "Sant Ravidas Nagar", "Deegh", 2095, "exact",
    "Sant Ravidas Nagar", "Gyanpur", "Sant Ravidas Nagar", "Gyanpur", 2096, "exact",
    "Sant Ravidas Nagar", "Suriyawan", "Sant Ravidas Nagar", "Suriyavan", 2097, "spelling",

    # =========================================================================
    # SHAHJAHNAPUR → SHAHJAHANPUR (District Code 171)
    # =========================================================================
    "Shahjahnapur", "Bhavlakheda", "Shahjahanpur", "Bhawal Khera", 2099, "spelling",
    "Shahjahnapur", "Daderol", "Shahjahanpur", "Dadrol", 2100, "spelling",
    "Shahjahnapur", "Jaitipur", "Shahjahanpur", "Jaitipur", 2101, "exact",
    "Shahjahnapur", "Jalalabad", "Shahjahanpur", "Jalalabad", 2102, "exact",
    "Shahjahnapur", "Kalan", "Shahjahanpur", "Kalan", 2103, "exact",
    "Shahjahnapur", "Kant", "Shahjahanpur", "Kanth", 2104, "spelling",
    "Shahjahnapur", "Katra - Khudaganj", "Shahjahanpur", "Khudaganj Katra", 2105, "spelling",
    "Shahjahnapur", "Khutar", "Shahjahanpur", "Khutar", 2106, "exact",
    "Shahjahnapur", "Madaanaapur", "Shahjahanpur", "Madnapur", 2107, "spelling",
    "Shahjahnapur", "Mirzapur", "Shahjahanpur", "Mirzapur", 2108, "exact",
    "Shahjahnapur", "Nigonhi", "Shahjahanpur", "Nigohi", 2109, "spelling",
    "Shahjahnapur", "Puwaya", "Shahjahanpur", "Powayan", 2110, "spelling",
    "Shahjahnapur", "Sindhauli", "Shahjahanpur", "Sindhauli", 2111, "exact",
    "Shahjahnapur", "Tilhar", "Shahjahanpur", "Tilhar", 2112, "exact",
    "Shahjahnapur", "Wanda", "Shahjahanpur", "Banda", 2098, "alt_name",

    # =========================================================================
    # SHRAVASTI (District Code 173)
    # =========================================================================
    "Shravasti", "Gilola", "Shravasti", "Gilaula", 2114, "spelling",
    "Shravasti", "Hariharpur Rani", "Shravasti", "Hariharpur Rani", 2115, "exact",
    "Shravasti", "Ikouna", "Shravasti", "Ekona", 2113, "spelling",
    "Shravasti", "Jamunha", "Shravasti", "Jamunaha", 2116, "spelling",
    "Shravasti", "Sirsia", "Shravasti", "Sirsiya", 2117, "spelling",

    # =========================================================================
    # SIDDHARTH NAGAR (District Code 174)
    # Urban wards are excluded (not block panchayats)
    # =========================================================================
    "Siddharth Nagar", "Badhni", "Siddharth Nagar", "Barhni", 2119, "spelling",
    "Siddharth Nagar", "Bansi", "Siddharth Nagar", "Bansi", 2118, "exact",
    "Siddharth Nagar", "Bhanwaapur", "Siddharth Nagar", "Bhanwapur", 2120, "spelling",
    "Siddharth Nagar", "Dumriaganj", "Siddharth Nagar", "Domariyaganj", 2122, "spelling",
    "Siddharth Nagar", "Itwa", "Siddharth Nagar", "Itwa", 2123, "exact",
    "Siddharth Nagar", "Jogiya", "Siddharth Nagar", "Jogia", 2124, "spelling",
    "Siddharth Nagar", "Khesarha", "Siddharth Nagar", "Khesraha", 2125, "spelling",
    "Siddharth Nagar", "Khuniyav", "Siddharth Nagar", "Khuniyaon", 2126, "spelling",
    "Siddharth Nagar", "Lotan Bazar", "Siddharth Nagar", "Lotan", 2127, "spelling",
    "Siddharth Nagar", "Mithwal", "Siddharth Nagar", "Mithwal", 2128, "exact",
    "Siddharth Nagar", "Naugadh", "Siddharth Nagar", "Naugarh", 2129, "spelling",
    "Siddharth Nagar", "Shohratgarh", "Siddharth Nagar", "Shoharatgarh", 2130, "spelling",
    "Siddharth Nagar", "UskaBazar", "Siddharth Nagar", "Uska Bazar", 2131, "spelling",

    # =========================================================================
    # SITAPUR (District Code 175)
    # =========================================================================
    "Sitapur", "Ailiya", "Sitapur", "Ailiya", 2132, "exact",
    "Sitapur", "Behata", "Sitapur", "Behta", 2133, "spelling",
    "Sitapur", "Biswan", "Sitapur", "Biswan", 2134, "exact",
    "Sitapur", "Gondlamau", "Sitapur", "Gondlamau", 2135, "exact",
    "Sitapur", "Hargaon", "Sitapur", "Hargaon", 2136, "exact",
    "Sitapur", "Kasmanda", "Sitapur", "Kasmanda", 2137, "exact",
    "Sitapur", "Khairbad", "Sitapur", "Khairabad", 2138, "spelling",
    "Sitapur", "Laharpur", "Sitapur", "Laharpur", 2139, "exact",
    "Sitapur", "Machhrehta", "Sitapur", "Machhrehta", 2140, "exact",
    "Sitapur", "Mahmudabad", "Sitapur", "Mahmudabad", 2141, "exact",
    "Sitapur", "Maholi", "Sitapur", "Maholi", 2142, "exact",
    "Sitapur", "Mishrikh", "Sitapur", "Misrikh", 2143, "spelling",
    "Sitapur", "Pahala", "Sitapur", "Pahala", 2144, "exact",
    "Sitapur", "Parsendi", "Sitapur", "Parsendi", 2145, "exact",
    "Sitapur", "Pisawan", "Sitapur", "Pisawan", 2146, "exact",
    "Sitapur", "Rampurmathura", "Sitapur", "Rampur Mathura", 2147, "spelling",
    "Sitapur", "Reusaa", "Sitapur", "Reusa", 2148, "spelling",
    "Sitapur", "Sakran", "Sitapur", "Sakran", 2149, "exact",
    "Sitapur", "Sidhauli", "Sitapur", "Sidhauli", 2150, "exact",

    # =========================================================================
    # SONBHADRA (District Code 176)
    # =========================================================================
    "Sonbhadra", "Chatra", "Sonbhadra", "Chatra", 2152, "exact",
    "Sonbhadra", "Chopan", "Sonbhadra", "Chopan", 2153, "exact",
    "Sonbhadra", "Duddhi", "Sonbhadra", "Dudhi", 2154, "spelling",
    "Sonbhadra", "Ghorawal", "Sonbhadra", "Ghorawal", 2155, "exact",
    "Sonbhadra", "Myorpur", "Sonbhadra", "Myorpur", 2156, "exact",
    "Sonbhadra", "Nagwan", "Sonbhadra", "Nagwa", 2157, "spelling",
    "Sonbhadra", "Robertsganj", "Sonbhadra", "Robertsganj", 2158, "exact",
    "Sonbhadra", "Vabhni", "Sonbhadra", "Babhani", 2151, "spelling",

    # =========================================================================
    # SULTANPUR (District Code 177)
    # =========================================================================
    "Sultanpur", "Akhandnagar", "Sultanpur", "Akhand Nagar", 2159, "spelling",
    "Sultanpur", "Baldirai", "Sultanpur", "Baldirai", 2161, "exact",
    "Sultanpur", "Bhaiyya", "Sultanpur", "Bhadaiya", 2162, "spelling",
    "Sultanpur", "Dhanpat Ganj", "Sultanpur", "Dhanpatganj", 2165, "spelling",
    "Sultanpur", "Doobepur", "Sultanpur", "Dubepur", 2167, "spelling",
    "Sultanpur", "Dostpur", "Sultanpur", "Dostpur", 2166, "exact",
    "Sultanpur", "Jaisinghpur", "Sultanpur", "Jaisinghpur", 2170, "exact",
    "Sultanpur", "Kadipur", "Sultanpur", "Kadipur", 2172, "exact",
    "Sultanpur", "Karaudi Kalan", "Sultanpur", "Karaudikala", 6897, "spelling",
    "Sultanpur", "Kudebhar", "Sultanpur", "Kurebhar", 2173, "spelling",
    "Sultanpur", "Kudwar", "Sultanpur", "Kurwar", 2174, "spelling",
    "Sultanpur", "Lambhua", "Sultanpur", "Lambhua", 2175, "exact",
    "Sultanpur", "Motigarpur", "Sultanpur", "Motigarpur", 2176, "exact",
    "Sultanpur", "Pratappur Kemaicha", "Sultanpur", "P.P.Kamaicha", 2178, "spelling",

    # =========================================================================
    # UNNAO (District Code 178)
    # =========================================================================
    "Unnao", "Asoha", "Unnao", "Asoha", 2182, "exact",
    "Unnao", "Auras", "Unnao", "Auras", 2183, "exact",
    "Unnao", "Bangarmau", "Unnao", "Bangarmau", 2184, "exact",
    "Unnao", "Bichiya", "Unnao", "Bichhiya", 2185, "spelling",
    "Unnao", "Bighapur", "Unnao", "Bighapur", 2186, "exact",
    "Unnao", "Fatehpur Chaurasi", "Unnao", "Fatehpur Chaurasi", 2187, "exact",
    "Unnao", "Hasan ganj", "Unnao", "Hasanganj", 2189, "spelling",
    "Unnao", "Hilauli", "Unnao", "Hilauli", 2190, "exact",
    "Unnao", "Miyoganj", "Unnao", "Mianganj", 2191, "spelling",
    "Unnao", "Nawabganj", "Unnao", "Nawabganj", 2192, "exact",
    "Unnao", "Purwa", "Unnao", "Purwa", 2193, "exact",
    "Unnao", "Safipur", "Unnao", "Safipur", 2194, "exact",
    "Unnao", "Si. Sirosi", "Unnao", "Sikandarpur Sarausi", 2196, "spelling",
    "Unnao", "Sikandarpur Karan", "Unnao", "Sikandarpur Karan", 2195, "exact",
    "Unnao", "Sumerpur", "Unnao", "Sumerpur", 2197, "exact",

    # =========================================================================
    # VARANASI (District Code 179)
    # =========================================================================
    "Varanasi", "Araji Lines", "Varanasi", "Arajiline", 2198, "spelling",
    "Varanasi", "Badagaon", "Varanasi", "Baragaon", 2199, "spelling",
    "Varanasi", "Chiraigaon", "Varanasi", "Chiraigaon", 2200, "exact",
    "Varanasi", "Cholapur", "Varanasi", "Cholapur", 2201, "exact",
    "Varanasi", "Harhua", "Varanasi", "Harahua", 2202, "spelling",
    "Varanasi", "Kashividyapeeth", "Varanasi", "Kashi Vidyapeeth", 2203, "spelling",
    "Varanasi", "Pindra", "Varanasi", "Pindra", 2204, "exact",
    "Varanasi", "Sewapuri", "Varanasi", "Sevapuri", 2205, "spelling"
)

cat("Manual crosswalk defined:", nrow(block_xwalk), "entries\n")

# =============================================================================
# STEP 4: Identify urban wards (not block panchayats - excluded)
# =============================================================================

# Siddharth Nagar has urban ward subdivisions that should be excluded
urban_wards <- tribble(
    ~elex_district, ~elex_block, ~reason,
    "Siddharth Nagar", "Vardpur No. 12", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 17", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 18", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 19", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 21", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 22", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 26", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 27", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 28", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 30", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 31", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 34", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 36", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 37", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 38", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 39", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 41", "urban_ward",
    "Siddharth Nagar", "Vardpur No. 42", "urban_ward",
    "Siddharth Nagar", "Wardpur No 09", "urban_ward",
    "Siddharth Nagar", "Wardpur No 10", "urban_ward",
    "Siddharth Nagar", "Wardpur No 11", "urban_ward",
    "Siddharth Nagar", "Wardpur No 13", "urban_ward",
    "Siddharth Nagar", "Wardpur No 14", "urban_ward",
    "Siddharth Nagar", "Wardpur No 15", "urban_ward",
    "Siddharth Nagar", "Wardpur No 16", "urban_ward",
    "Siddharth Nagar", "Wardpur No 20", "urban_ward",
    "Siddharth Nagar", "Wardpur No 23", "urban_ward",
    "Siddharth Nagar", "Wardpur No 24", "urban_ward",
    "Siddharth Nagar", "Wardpur No 25", "urban_ward",
    "Siddharth Nagar", "Wardpur No 29", "urban_ward",
    "Siddharth Nagar", "Wardpur No 32", "urban_ward",
    "Siddharth Nagar", "Wardpur No 33", "urban_ward",
    "Siddharth Nagar", "Wardpur No 35", "urban_ward",
    "Siddharth Nagar", "Wardpur No 40", "urban_ward",
    "Siddharth Nagar", "Wardpur No 43", "urban_ward"
)

cat("Urban wards excluded:", nrow(urban_wards), "\n")

# =============================================================================
# STEP 5: Validate against LGD blocks
# =============================================================================

cat("\nValidating LGD block codes...\n")

valid_codes <- lgd_blocks$block_code

invalid_codes <- block_xwalk %>%
    filter(!lgd_block_code %in% valid_codes)

if (nrow(invalid_codes) > 0) {
    cat("ERROR: Invalid LGD block codes found:\n")
    print(invalid_codes)
    stop("All block codes must exist in LGD reference data!")
} else {
    cat("All LGD block codes validated OK\n")
}

# =============================================================================
# STEP 6: Load election data and verify coverage
# =============================================================================

cat("\nLoading election data to verify coverage...\n")

up_05_10 <- read_parquet(here("data/up/up_05_10.parquet"))

elex_blocks <- up_05_10 %>%
    select(district_name_eng_2010, block_name_eng_2010) %>%
    filter(!is.na(block_name_eng_2010)) %>%
    distinct() %>%
    arrange(district_name_eng_2010, block_name_eng_2010)

cat("Total unique election blocks:", nrow(elex_blocks), "\n")

# Check coverage (excluding urban wards)
missing <- elex_blocks %>%
    anti_join(
        block_xwalk,
        by = c("district_name_eng_2010" = "elex_district",
               "block_name_eng_2010" = "elex_block")
    ) %>%
    anti_join(
        urban_wards,
        by = c("district_name_eng_2010" = "elex_district",
               "block_name_eng_2010" = "elex_block")
    )

if (nrow(missing) > 0) {
    cat("\nWARNING: Missing blocks in crosswalk (", nrow(missing), "):\n")
    print(missing %>% arrange(district_name_eng_2010, block_name_eng_2010))
} else {
    cat("\nCoverage check: 100% of election blocks mapped (excluding urban wards)\n")
}

# =============================================================================
# STEP 7: Summary statistics
# =============================================================================

cat("\n=== CROSSWALK SUMMARY ===\n")
cat("Total entries:", nrow(block_xwalk), "\n")
cat("Urban wards excluded:", nrow(urban_wards), "\n")

cat("\nBy match type:\n")
print(block_xwalk %>% count(match_type) %>% arrange(desc(n)))

cat("\nBy LGD district (top 15):\n")
print(block_xwalk %>%
    count(lgd_district) %>%
    arrange(desc(n)) %>%
    head(15))

# =============================================================================
# STEP 8: Save outputs
# =============================================================================

write_csv(block_xwalk, here("data/crosswalks/up_block_xwalk.csv"))
cat("\nSaved: data/crosswalks/up_block_xwalk.csv\n")

write_csv(urban_wards, here("data/crosswalks/up_urban_wards_excluded.csv"))
cat("Saved: data/crosswalks/up_urban_wards_excluded.csv\n")

cat("\n=== Done ===\n")
