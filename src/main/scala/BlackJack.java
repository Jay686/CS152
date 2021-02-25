package javaDemo;
import java.util.Random;

public class BlackJack {

	public static void main(String[] args) {

		Random gen = new Random(System.currentTimeMillis());

		// create a shuffled deck of 52 "cards"
		Integer[] cards = new Integer[52];
		for(int i = 0; i < 52; i++) cards[i] = (gen.nextBoolean())?gen.nextInt(11): -1;

		int total = 0; // = sum of cards in player's hand

		for(int i = 0; i < 52; i++) {
			if (cards[i] <= 0) continue; // skip Jokers!
			total += cards[i];
			if (21 <= total) break;
		}

		System.out.println("total = " + total);

	}

}
