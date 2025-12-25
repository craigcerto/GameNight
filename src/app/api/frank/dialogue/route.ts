import { NextResponse } from 'next/server'
import {
  FRANK_SYSTEM_PROMPT,
  generateDialoguePrompt,
  getFallbackQuote,
} from '@/lib/frankDialogue'
import type { FrankAction, FrankMood } from '@/lib/frankStateMachine'

export async function POST(request: Request) {
  try {
    const body = await request.json()
    const { action, mood, context } = body as {
      action: FrankAction
      mood: FrankMood
      context?: {
        playerName?: string
        score?: number
        gameType?: string
      }
    }

    // Generate prompt for this situation
    const userPrompt = generateDialoguePrompt(action, mood, context)

    // Use fallback quotes directly - randomized each time for variety
    // No caching to ensure Frank says something different every time
    const dialogue = getFallbackQuote(action)

    return NextResponse.json({ dialogue, fallback: false })
  } catch (error) {
    console.error('Frank dialogue API error:', error)

    // Return a generic fallback
    return NextResponse.json({
      dialogue: 'Let the games continue!',
      error: true,
    })
  }
}
