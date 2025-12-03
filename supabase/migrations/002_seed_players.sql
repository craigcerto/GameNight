-- Seed initial players (your game night crew!)
-- Run this after the schema migration

INSERT INTO players (name, nickname, color, avatar_url) VALUES
  ('Gloria', 'Won Non', '#321D71', NULL),
  ('Paul', 'Pah-OOL', '#8C1A10', NULL),
  ('Lauren', 'Gerald McBoingBoing', '#48752C', NULL),
  ('Craig', 'Ya Boii', '#2854C5', NULL),
  ('Frank', 'Franconia Springfield', '#964B00', NULL),
  ('Dave', 'Graham', '#000000', NULL),
  ('Amy', 'Amy', '#CEA8BC', NULL),
  ('Stacy', 'Stacy', '#7CA7D8', NULL),
  ('Keith', 'Keith', '#0E2787', NULL)
ON CONFLICT DO NOTHING;
