-- Update family members with new casino-themed avatars
-- This migration updates the 6 family members with their new pixel art avatars

-- Update Lauren
UPDATE players
SET avatar_url = '/images/players/lauren.png',
    color = '#FF10F0'  -- Magenta to match her "Wild Card" theme
WHERE name = 'Lauren';

-- Update Craig
UPDATE players
SET avatar_url = '/images/players/craig.png',
    color = '#00FFFF'  -- Cyan to match his "Strategic Analyst" theme
WHERE name = 'Craig';

-- Update Gloria
UPDATE players
SET avatar_url = '/images/players/gloria.png',
    color = '#FFD700'  -- Gold to match her "Card Counter" theme
WHERE name = 'Gloria';

-- Update Paul
UPDATE players
SET avatar_url = '/images/players/paul.png',
    color = '#00FFFF'  -- Cyan to match his "Poker Face" theme
WHERE name = 'Paul';

-- Update Amber
UPDATE players
SET avatar_url = '/images/players/amber.png',
    color = '#FF10F0'  -- Hot pink to match her "Lucky Gambler" theme
WHERE name = 'Amber';

-- Update Gareth
UPDATE players
SET avatar_url = '/images/players/gareth.png',
    color = '#8B00FF'  -- Electric purple to match his "All-In Risk Taker" theme
WHERE name = 'Gareth';

-- If these players don't exist yet, insert them
INSERT INTO players (name, avatar_url, color)
SELECT 'Lauren', '/images/players/lauren.png', '#FF10F0'
WHERE NOT EXISTS (SELECT 1 FROM players WHERE name = 'Lauren');

INSERT INTO players (name, avatar_url, color)
SELECT 'Craig', '/images/players/craig.png', '#00FFFF'
WHERE NOT EXISTS (SELECT 1 FROM players WHERE name = 'Craig');

INSERT INTO players (name, avatar_url, color)
SELECT 'Gloria', '/images/players/gloria.png', '#FFD700'
WHERE NOT EXISTS (SELECT 1 FROM players WHERE name = 'Gloria');

INSERT INTO players (name, avatar_url, color)
SELECT 'Paul', '/images/players/paul.png', '#00FFFF'
WHERE NOT EXISTS (SELECT 1 FROM players WHERE name = 'Paul');

INSERT INTO players (name, avatar_url, color)
SELECT 'Amber', '/images/players/amber.png', '#FF10F0'
WHERE NOT EXISTS (SELECT 1 FROM players WHERE name = 'Amber');

INSERT INTO players (name, avatar_url, color)
SELECT 'Gareth', '/images/players/gareth.png', '#8B00FF'
WHERE NOT EXISTS (SELECT 1 FROM players WHERE name = 'Gareth');
