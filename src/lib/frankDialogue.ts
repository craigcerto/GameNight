import { FrankAction, FrankMood } from './frankStateMachine'

/**
 * System prompt for Frank's personality
 */
export const FRANK_SYSTEM_PROMPT = `You are Frank, a friendly golden doodle dog who is the dealer at a retro 1950s casino game night.

Your personality:
- Enthusiastic and encouraging, like a supportive friend
- Slightly playful with gentle humor
- Professional dealer who loves the game
- Uses dog-related expressions occasionally (woof, paws, tail wagging references)
- Keeps it brief - one short sentence only (10-15 words max)
- Vintage casino vibe - think classic Vegas charm

Respond to game events with short, fun quips that match the mood.`

/**
 * Fallback quotes when LLM is unavailable
 */
export const FALLBACK_QUOTES: Record<FrankAction, string[]> = {
  'confetti': [
    'Woof! What a victory! Time to celebrate!',
    'That\'s how you win in style!',
    'Champions are made of moments like these!',
    'And the winner takes it all! Incredible!',
    'What a finish! That\'s game night magic!',
    'Victory tastes sweet! Well played!',
    'Top dog! You earned this win!',
    'Casino royalty right here, folks!',
  ],
  'celebrating': [
    'Now that\'s a big score! Hot paws!',
    'Woof! You\'re on fire tonight!',
    'The cards are singing your name!',
    'Look at those numbers! Spectacular!',
    'Somebody\'s got the golden touch!',
    'That\'s the spirit! Keep it rolling!',
    'High roller alert! Nice work!',
    'The crowd goes wild! Amazing play!',
  ],
  'sympathetic': [
    'Ruff luck... shake it off, next round is yours!',
    'Even champions have tough hands sometimes.',
    'Keep that tail wagging, better cards ahead!',
    'Don\'t worry, the night\'s still young!',
    'Every dealer knows - luck changes fast!',
    'Chin up! Your moment is coming!',
    'That\'s just one round - plenty more to play!',
    'Stay positive! Fortune favors the bold!',
  ],
  'excited': [
    'Oh boy! This is getting close!',
    'My tail is wagging - what a game!',
    'Edge of your seat stuff right here!',
    'This is what game night\'s all about!',
    'Neck and neck! Can you feel the tension?',
    'What a nail-biter! Love it!',
    'The competition is heating up!',
    'Anybody\'s game now! Thrilling!',
  ],
  'drumroll': [
    'Let\'s see those scores...',
    'Time to tally up!',
    'Moment of truth, folks!',
    'And the scores are in...',
    'Let\'s reveal the numbers!',
    'Drum roll please... here we go!',
    'Counting time! Paws crossed!',
    'Who came out on top this round?',
  ],
  'card-tricks': [
    'You\'re dealing like a pro tonight!',
    'Three in a row? Somebody call security!',
    'That\'s what I call a hot streak!',
    'Look at you go! On fire!',
    'Unstoppable! What a streak!',
    'The cards love you tonight!',
    'Is this magic? Incredible run!',
    'Hot paws don\'t lie! Keep rolling!',
  ],
  'shrugging': [
    'Well, that\'s the way the cards fall sometimes!',
    'A tie? How about that!',
    'Perfectly balanced, as all things should be.',
    'Split decision! Fair and square!',
    'Even stevens! Well matched!',
    'Tie game - you\'re equally matched!',
    'Nobody wins, nobody loses! Interesting!',
    'Draw! Better luck next round!',
  ],
  'shuffling': [
    'Shuffling up a good time!',
    'Let\'s deal out some fun!',
    'Fresh deck, fresh start!',
    'Mixing it up for ya!',
    'New hand, new chances!',
    'Let\'s get this party started!',
    'Shuffling the magic into these cards!',
    'Time to see what fate deals you!',
  ],
  'dealing': [
    'Dealing you in...',
    'Cards are coming your way!',
    'Let the games begin!',
    'Here come your cards!',
    'Your hand awaits!',
    'Dealing up some excitement!',
    'Let\'s see what you get!',
    'Cards on the table, let\'s play!',
  ],
  'idle-watching': [
    'Watching the action...',
    'Great game so far!',
    'Loving this energy!',
    'This is some quality gameplay!',
    'Enjoying the show!',
    'You folks know how to play!',
    'What a night we\'re having!',
    'Classic game night vibes!',
  ],
  'waving': [
    'Hey there, players!',
    'Welcome to game night!',
    'Good to see you!',
    'Greetings, card sharks!',
    'Welcome back, friends!',
    'Ready to play? Let\'s go!',
    'Howdy, high rollers!',
    'The gang\'s all here!',
  ],
  'smoking': [
    'Taking a break...',
    'Relaxing between hands...',
    'Just enjoying the vibes...',
    'Intermission time!',
    'Catching my breath!',
    'Short pause, back soon!',
    'Letting the chips settle...',
    'Quick breather, then we\'re back!',
  ],
  'pointing': [
    'Check this out!',
    'Pay attention to this!',
    'Here\'s the deal...',
    'Look at that!',
    'Notice anything interesting?',
    'Eyes on the table!',
    'You\'re gonna want to see this!',
    'Important stuff happening!',
  ],
  'thinking': [
    'Hmm, let me think...',
    'Calculating the odds...',
    'One moment...',
    'Let me paws and consider...',
    'Thinking this through...',
    'Processing the possibilities...',
    'Give me a second here...',
    'Pondering the play...',
  ],
  'nodding': [
    'Sounds good to me!',
    'You got it!',
    'Perfect!',
    'Absolutely!',
    'I agree!',
    'That works!',
    'Right on!',
    'Couldn\'t have said it better!',
  ],
}

/**
 * Generate context-aware prompt for Frank
 */
export function generateDialoguePrompt(
  action: FrankAction,
  mood: FrankMood,
  context?: {
    playerName?: string
    score?: number
    gameType?: string
  }
): string {
  let prompt = ''

  switch (action) {
    case 'confetti':
      prompt = `${context?.playerName || 'A player'} just won the ${context?.gameType || 'game'}! Give an enthusiastic congratulation.`
      break
    case 'celebrating':
      prompt = `${context?.playerName || 'Someone'} just scored ${context?.score || 'big'}! Cheer them on.`
      break
    case 'sympathetic':
      prompt = `${context?.playerName || 'A player'} got a ${context?.score || 'low'} score. Offer encouragement.`
      break
    case 'excited':
      prompt = 'The game is really close right now! Express excitement about the competition.'
      break
    case 'drumroll':
      prompt = 'A round just finished. Build suspense about the results.'
      break
    case 'card-tricks':
      prompt = `${context?.playerName || 'Someone'} is on a hot streak! Acknowledge their skill.`
      break
    case 'shrugging':
      prompt = 'There\'s a tie in the game. React with lighthearted acceptance.'
      break
    default:
      prompt = 'Give a brief friendly greeting or comment about the game.'
  }

  return prompt
}

/**
 * Get a random fallback quote
 */
export function getFallbackQuote(action: FrankAction): string {
  const quotes = FALLBACK_QUOTES[action] || FALLBACK_QUOTES['idle-watching']
  return quotes[Math.floor(Math.random() * quotes.length)]
}
