const { createClient } = require('@supabase/supabase-js');
require('dotenv').config({ path: '.env.local' });

const supabase = createClient(
  process.env.NEXT_PUBLIC_SUPABASE_URL,
  process.env.SUPABASE_SERVICE_ROLE_KEY
);

async function updateAvatars() {
  console.log('🎨 Updating family avatars...\n');

  const players = [
    { name: 'Lauren', avatar_url: '/images/players/lauren.png', color: '#FF10F0' },
    { name: 'Craig', avatar_url: '/images/players/craig.png', color: '#00FFFF' },
    { name: 'Gloria', avatar_url: '/images/players/gloria.png', color: '#FFD700' },
    { name: 'Paul', avatar_url: '/images/players/paul.png', color: '#00FFFF' },
    { name: 'Amber', avatar_url: '/images/players/amber.png', color: '#FF10F0' },
    { name: 'Gareth', avatar_url: '/images/players/gareth.png', color: '#8B00FF' },
  ];

  for (const player of players) {
    // Check if player exists
    const { data: existing } = await supabase
      .from('players')
      .select('id')
      .eq('name', player.name)
      .single();

    if (existing) {
      // Update existing player
      const { error } = await supabase
        .from('players')
        .update({
          avatar_url: player.avatar_url,
          color: player.color,
        })
        .eq('name', player.name);

      if (error) {
        console.error(`❌ Error updating ${player.name}:`, error.message);
      } else {
        console.log(`✅ Updated ${player.name} with new avatar and color`);
      }
    } else {
      // Insert new player
      const { error } = await supabase
        .from('players')
        .insert({
          name: player.name,
          avatar_url: player.avatar_url,
          color: player.color,
        });

      if (error) {
        console.error(`❌ Error inserting ${player.name}:`, error.message);
      } else {
        console.log(`✅ Created ${player.name} with new avatar and color`);
      }
    }
  }

  console.log('\n🎉 Avatar update complete!');
}

updateAvatars().catch(console.error);
