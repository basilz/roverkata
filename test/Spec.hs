{-# LANGUAGE BlockArguments #-}

import Lib
import Test.Hspec ( hspec, describe, it, shouldBe )

rStart :: Rover
rStart = rover 9 1 2 North

main :: IO ()
main = hspec do
    describe "Rover kata" do
        it "new instance" do
            rover 9 1 2 North `shouldBe` Rover (Position (Coordinate 1 9) (Coordinate 2 9), North) 
    describe "Single commands" do
        it "when command is F" do
            shouldBe
                do move "F" rStart
                do rover 9 1 3 North
        it "when command is B" do
            shouldBe
                do move "B" rStart
                do rover 9 1 1 North
        it "when command is L" do
            shouldBe
                do move "L" rStart
                do rover 9 1 2 West
        it "when command is R" do
            shouldBe
                do move "R" rStart
                do rover 9 1 2 East
    describe "Ignore Case" do
        it "ignore case F" do
            shouldBe
                do move "f" rStart
                do rover 9 1 3 North
        it "ignore case B" do
            shouldBe
                do move "b" rStart
                do rover 9 1 1 North
        it "ignore case L" do
            shouldBe
                do move "l" rStart
                do rover 9 1 2 West
        it "ignore case R" do
            shouldBe
                do move "r" rStart
                do rover 9 1 2 East
    describe "Unknown Command" do
        it "ignore unknown command" do
            shouldBe
                do move "X" rStart
                do rover 9 1 2 North
    describe "Multiple commands" do
        it "receives multiple commands" do
            shouldBe
                do move "FFFRRBFFF" rStart
                do rover 9 1 3 South
        it "wraps up Forward North" do
            shouldBe
                do move "FFFFFFFFFF" rStart
                do rover 9 1 2 North
        it "wraps up Forward South" do
            shouldBe
                do move "RRFFFFFFFFFF" rStart
                do rover 9 1 2 South
        it "wraps up Forward West" do
            shouldBe
                do move "LFFFFFFFFFF" rStart
                do rover 9 1 2 West
        it "wraps up Forward East" do
            shouldBe
                do move "RFFFFFFFFFF" rStart
                do rover 9 1 2 East
    describe "Obstacles" do
        it "should stop when an obstacle is found" do
            1 `shouldBe` 2
        it "return NOK when obstacle is found" do
            1 `shouldBe` 2