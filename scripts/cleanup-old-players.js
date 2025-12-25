const { createClient } = require('@supabase/supabase-js');
const fs = require('fs');
const path = require('path');
require('dotenv').config({ path: '.env.local' });

const supabase = createClient(
  process.env.NEXT_PUBLIC_SUPABASE_URL,
  process.env.SUPABASE_SERVICE_ROLE_KEY
);

async function cleanupOldPlayers() {
  console.log('🧹 Cleaning up old players...\n');

  // Keep only these 6 family members
  const familyMembers = ['Lauren', 'Craig', 'Gloria', 'Paul', 'Amber', 'Gareth'];

  // Get all current players
  const { data: allPlayers, error: fetchError } = await supabase
    .from('players')
    .select('id, name, avatar_url');

  if (fetchError) {
    console.error('❌ Error fetching players:', fetchError.message);
    return;
  }

  console.log(`Found ${allPlayers.length} total players`);
  console.log(`Keeping ${familyMembers.length} family members: ${familyMembers.join(', ')}\n`);

  // Delete players not in family list
  for (const player of allPlayers) {
    if (!familyMembers.includes(player.name)) {
      const { error: deleteError } = await supabase
        .from('players')
        .delete()
        .eq('id', player.id);

      if (deleteError) {
        console.error(`❌ Error deleting ${player.name}:`, deleteError.message);
      } else {
        console.log(`🗑️  Deleted player: ${player.name}`);
      }
    }
  }

  // Clean up old avatar files
  console.log('\n🧹 Cleaning up old avatar files...\n');

  const avatarsDir = path.join(__dirname, '../public/images/players');
  const keepFiles = [
    'lauren.png',
    'craig.png',
    'gloria.png',
    'paul.png',
    'amber.png',
    'gareth.png',
    'frank.png', // Keep Frank - he's the dealer mascot
  ];

  const files = fs.readdirSync(avatarsDir);

  for (const file of files) {
    if (!keepFiles.includes(file) && file.endsWith('.png')) {
      const filePath = path.join(avatarsDir, file);
      fs.unlinkSync(filePath);
      console.log(`🗑️  Deleted old avatar: ${file}`);
    }
  }

  console.log('\n✨ Cleanup complete!');
  console.log(`\nRemaining players in database: ${familyMembers.join(', ')}`);
  console.log(`Remaining avatar files: ${keepFiles.join(', ')}`);
}

cleanupOldPlayers().catch(console.error);
