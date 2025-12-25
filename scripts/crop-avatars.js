const sharp = require('sharp');
const path = require('path');

async function cropAvatars() {
  const inputPath = path.join(__dirname, '../public/images/avatars/family-grid.png');
  const outputDir = path.join(__dirname, '../public/images/players');

  const metadata = await sharp(inputPath).metadata();
  console.log('Grid dimensions:', metadata.width, 'x', metadata.height);

  // The grid has borders and labels. Need to find the actual content boxes
  // Looking at the image: each cell appears to have a colored border and text at bottom
  // Let's measure more precisely

  // Estimate based on visual inspection:
  // - Grid has outer padding
  // - Each cell has inner border (magenta/cyan colored boxes)
  // - Text labels at bottom of each cell

  // Precise measurements by analyzing the grid structure
  // Grid is 1024x1024 total
  // Looking at the image closely:
  // - Outer padding/border
  // - Each cell has a colored neon border (magenta/cyan)
  // - Text label at bottom of each cell
  // - Need to crop INSIDE the neon borders

  const outerPadding = 47;    // Black outer margin
  const cellBorderWidth = 8;   // The neon colored border thickness (increased)
  const labelHeight = 42;      // Text at bottom including padding
  const cellGap = 10;         // Gap between cells

  // Total grid content area
  const gridWidth = metadata.width - (outerPadding * 2);
  const gridHeight = metadata.height - (outerPadding * 2);

  // Each cell size (including borders)
  const cellWidth = Math.floor(gridWidth / 2);
  const cellHeight = Math.floor(gridHeight / 3);

  console.log('Cell dimensions (with borders):', cellWidth, 'x', cellHeight);

  // Define exact crop positions - inside the neon borders
  const avatars = [
    { name: 'lauren', col: 0, row: 0 },
    { name: 'craig', col: 1, row: 0 },
    { name: 'gloria', col: 0, row: 1 },
    { name: 'paul', col: 1, row: 1 },
    { name: 'amber', col: 0, row: 2 },
    { name: 'gareth', col: 1, row: 2 },
  ];

  for (const avatar of avatars) {
    // Calculate cell position
    const cellLeft = outerPadding + (avatar.col * cellWidth);
    const cellTop = outerPadding + (avatar.row * cellHeight);

    // Crop inside the neon border
    const left = cellLeft + cellBorderWidth + 2;
    const top = cellTop + cellBorderWidth + 2;
    const width = cellWidth - (cellBorderWidth * 2) - cellGap;
    const height = cellHeight - labelHeight - (cellBorderWidth * 2) - 4;

    const outputPath = path.join(outputDir, `${avatar.name}.png`);

    console.log(`Cropping ${avatar.name}: left=${left}, top=${top}, width=${width}, height=${height}`);

    await sharp(inputPath)
      .extract({
        left: Math.floor(left),
        top: Math.floor(top),
        width: Math.floor(width),
        height: Math.floor(height),
      })
      .resize(512, 512, { fit: 'cover', position: 'center' })
      .toFile(outputPath);

    console.log(`✅ Cropped ${avatar.name}.png`);
  }

  console.log('\n🎉 All avatars cropped successfully!');
}

cropAvatars().catch(console.error);
